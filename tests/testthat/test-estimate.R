theo_model <- function() {
  return(campsismod::read.campsis(system.file("extdata", "theo_sd_model.json", package = "campsisfit")))
}

# Reference model from the nlmixr2 documentation
theo_reference <- function() {
  ini({
    lCl <- 1.6
    lVc <- log(90)
    lKA <- 0.1
    prop.err <- c(0, 0.2, 1)
    eta.Cl ~ 0.1
    eta.Vc ~ 0.1
    eta.KA ~ 0.1
  })
  model({
    Cl <- exp(lCl + eta.Cl)
    Vc <- exp(lVc + eta.Vc)
    KA <- exp(lKA + eta.KA)
    kel <- Cl / Vc
    d / dt(depot) <- -KA * depot
    d / dt(centr) <- KA * depot - kel * centr
    cp <- centr / Vc
    cp ~ prop(prop.err)
  })
}

quietly <- function(expr) {
  return(suppressMessages(suppressWarnings(expr)))
}

# No optimisation: objective function evaluated at the initial estimates
no_iteration_control <- function() {
  return(list(print = 0, maxOuterIterations = 0, covMethod = ""))
}

get_theta <- function(model, name) {
  return(model |> campsismod::find(campsismod::Theta(name = name)))
}

test_that("Theophylline model is exported to nlmixr2 without warnings", {
  expect_no_warning(nlmod <- campsismod::export(theo_model(), dest = "nlmixr2"))
  expect_equal(
    nlmod@ini,
    c(
      "THETA_CL <- 1.6; label(\"log Cl (L/hr)\")",
      "THETA_VC <- 4.49980967033027; label(\"log Vc (L)\")",
      "THETA_KA <- 0.1; label(\"log Ka (1/hr)\")",
      "THETA_PROP_ERR <- c(0, 0.2, 1); label(\"Proportional residual error (SD)\")",
      "ETA_CL ~ 0.1",
      "ETA_VC ~ 0.1",
      "ETA_KA ~ 0.1"
    )
  )
  expect_true("CP ~ prop(THETA_PROP_ERR) # Observed plasma concentration" %in% nlmod@model)
})

test_that("Arguments of estimate are checked", {
  expect_error(estimate(theo_model(), dataset = "data"), regexp = "must be a data frame")
  expect_error(estimate(theo_model(), dataset = data.frame(), settings = list()), regexp = "EstimationSettings object")
  expect_error(estimate("model", dataset = data.frame()), class = "S7_error_method_not_found")
})

test_that("Campsis and reference models have the same objective function at initial estimates", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  settings <- EstimationSettings(control = no_iteration_control())
  results <- quietly(estimate(theo_model(), nlmixr2data::theo_sd, settings))
  ref <- quietly(nlmixr2est::nlmixr2(theo_reference, nlmixr2data::theo_sd, est = "focei", control = settings@control))
  expect_equal(results@fit$objf, ref$objf, tolerance = 1e-8)
})

test_that("FOCEi estimation works on theo_sd and the model is updated", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  settings <- read_settings(system.file("extdata", "settings_focei.json", package = "campsisfit"))
  results <- quietly(estimate(theo_model(), nlmixr2data::theo_sd, settings))
  fit <- results@fit

  expect_true(S7::S7_inherits(results, EstimationResults))
  expect_s3_class(fit, "nlmixr2FitData")
  expect_equal(fit$est, "focei")
  expect_equal(results@settings, settings)

  # Optimizer paths differ slightly from the reference model (OFV 133.86)
  expect_equal(fit$objf, 133.86, tolerance = 0.1 / 133.86)
  expect_equal(unname(fit$theta), c(1.03, 3.47, 0.39, 0.16), tolerance = 0.05)

  # Bounds of THETA_PROP_ERR are used by nlmixr2
  bounds <- fit$iniDf[fit$iniDf$name == "THETA_PROP_ERR", c("lower", "upper")]
  expect_equal(unlist(bounds), c(lower = 0, upper = 1))

  # Estimated values in the Campsis model, bounds are kept
  model <- results@model
  thetas <- vapply(c("CL", "VC", "KA", "PROP_ERR"), function(x) get_theta(model, x)@value, numeric(1))
  expect_equal(unname(thetas), unname(fit$theta))
  expect_equal(get_theta(model, "PROP_ERR")@min, 0)
  expect_equal(get_theta(model, "PROP_ERR")@max, 1)
  expect_equal(campsismod::rxode_matrix(model, type = "omega"), fit$omega)

  # The updated model reproduces the final objective function
  reevaluated <- quietly(estimate(model, nlmixr2data::theo_sd, EstimationSettings(control = no_iteration_control())))
  expect_equal(reevaluated@fit$objf, fit$objf, tolerance = 0.01 / fit$objf)

  # Exact OFV is platform-dependent (BLAS/compiler), its value is checked above
  expect_output(print(results), paste("Objective function value:", format(fit$objf, nsmall = 3)), fixed = TRUE)
})

test_that("SAEM estimation works and control options are forwarded", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  settings <- EstimationSettings(method = "saem", control = list(nBurn = 20, nEm = 20, print = 0))
  results <- quietly(estimate(theo_model(), nlmixr2data::theo_sd, settings))
  expect_equal(results@fit$est, "saem")
  expect_equal(results@fit$control$mcmc$niter, c(20, 20))
  expect_output(print(results), "Estimation method: saem")
})
