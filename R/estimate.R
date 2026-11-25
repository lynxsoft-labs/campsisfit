#_______________________________________________________________________________
#----                              estimate                                 ----
#_______________________________________________________________________________

# S4 class from campsismod, used for S7 dispatch
campsis_model_class <- methods::getClass("campsis_model", where = asNamespace("campsismod"))

#' Estimate the parameters of a model.
#'
#' The Campsis model is exported to nlmixr2 and fitted on the dataset with the
#' method and control options given in the settings. The estimated THETAs and
#' OMEGAs are written back into the Campsis model.
#'
#' @param model Campsis model
#' @param dataset data frame or tibble in the nlmixr2 data format (columns
#'   ID, TIME, DV, AMT, EVID, CMT, etc.)
#' @param settings estimation settings, see [EstimationSettings()]
#' @param ... extra arguments, not used
#' @return an [EstimationResults()] object, holding the Campsis model with the
#'   estimated parameters (`@model`), the settings (`@settings`) and the
#'   nlmixr2 fit (`@fit`)
#' @export
#' @examplesIf interactive() && requireNamespace("nlmixr2data", quietly = TRUE)
#' file <- system.file("extdata", "theo_sd_model.json", package = "campsisfit")
#' model <- campsismod::read.campsis(file)
#' results <- estimate(model, nlmixr2data::theo_sd, EstimationSettings(method = "focei"))
#' results
#' results@model
estimate <- S7::new_generic(
  "estimate",
  dispatch_args = "model",
  function(model, dataset, settings = EstimationSettings(), ...) {
    S7::S7_dispatch()
  }
)

S7::method(estimate, campsis_model_class) <- function(model, dataset, settings = EstimationSettings(), ...) {
  if (!is.data.frame(dataset)) {
    stop("Argument dataset must be a data frame")
  }
  if (!S7::S7_inherits(settings, EstimationSettings)) {
    stop("Argument settings must be an EstimationSettings object")
  }
  fit <- nlmixr2est::nlmixr2(
    object = nlmixr_function(model),
    data = dataset,
    est = settings@method,
    control = settings@control
  )
  return(EstimationResults(
    model = update_model_parameters(model, theta = fit$theta, omega = fit$omega),
    settings = settings,
    fit = fit
  ))
}

#' Get the nlmixr2 model function of a Campsis model.
#'
#' @param model Campsis model
#' @return a function with `ini()` and `model()` blocks, understood by nlmixr2
#' @keywords internal
nlmixr_function <- function(model) {
  code <- campsismod::export(model, dest = "nlmixr2") |>
    campsismod::to_string()
  return(eval(parse(text = code), envir = globalenv()))
}
