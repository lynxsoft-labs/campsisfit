types_model <- function() {
  return(
    campsismod::CampsisModel() |>
      campsismod::add(campsismod::Theta(name = "CL", index = 1, value = 3)) |>
      campsismod::add(campsismod::Theta(name = "UNUSED", index = 2, value = 1)) |>
      campsismod::add(campsismod::Omega(name = "CL", index = 1, index2 = 1, value = 25, type = "cv%")) |>
      campsismod::add(campsismod::Omega(name = "VC", index = 2, index2 = 2, value = 0.3, type = "sd")) |>
      campsismod::add(campsismod::Omega(name = "CL_VC", index = 2, index2 = 1, value = 0.5, type = "cor")) |>
      campsismod::add(campsismod::Omega(name = "KA", index = 3, index2 = 3, value = 0.2, type = "cv")) |>
      campsismod::add(campsismod::Omega(name = "Q", index = 4, index2 = 4, value = 0.1, type = "var")) |>
      campsismod::add(campsismod::Sigma(name = "RUV", index = 1, index2 = 1, value = 1, type = "var"))
  )
}

get_value <- function(model, x) {
  return((model |> campsismod::find(x))@value)
}

test_that("Estimated parameters are written back and keep their type", {
  etas <- c("ETA_CL", "ETA_VC", "ETA_KA")
  omega <- matrix(c(0.09, 0.03, 0, 0.03, 0.04, 0, 0, 0, 0.16), nrow = 3, dimnames = list(etas, etas))
  model <- campsisfit:::update_model_parameters(types_model(), theta = c(THETA_CL = 5), omega = omega)

  expect_equal(model |> get_value(campsismod::Theta(name = "CL")), 5)
  expect_equal(model |> get_value(campsismod::Omega(name = "CL")), 100 * sqrt(exp(0.09) - 1))
  expect_equal(model |> get_value(campsismod::Omega(name = "VC")), 0.2)
  expect_equal(model |> get_value(campsismod::Omega(name = "CL_VC")), 0.03 / sqrt(0.09 * 0.04))
  expect_equal(model |> get_value(campsismod::Omega(name = "KA")), sqrt(exp(0.16) - 1))
  expect_equal((model |> campsismod::find(campsismod::Omega(name = "CL")))@type, "cv%")

  # Standardised values are the estimates
  expect_equal(campsismod::rxode_matrix(model, type = "omega")[etas, etas], omega)

  # Parameters without estimate are left unchanged
  expect_equal(model |> get_value(campsismod::Theta(name = "UNUSED")), 1)
  expect_equal(model |> get_value(campsismod::Omega(name = "Q")), 0.1)
  expect_equal(model |> get_value(campsismod::Sigma(name = "RUV")), 1)
})

test_that("Estimation results only accept a Campsis model", {
  expect_error(EstimationResults(model = "model"), regexp = "@model must be")
  results <- EstimationResults(model = types_model(), settings = EstimationSettings(), fit = list(objf = 100))
  expect_output(print(results), "Estimation method: focei")
  expect_output(print(results), "Objective function value: 100")
})
