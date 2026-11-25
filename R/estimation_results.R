#' @include estimate.R estimation_settings.R
NULL

#_______________________________________________________________________________
#----                          Estimation results                           ----
#_______________________________________________________________________________

#' Estimation results.
#'
#' Object returned by [estimate()].
#'
#' @param model Campsis model, with the estimated parameter values
#' @param settings estimation settings, see [EstimationSettings()]
#' @param fit nlmixr2 fit object
#' @return an `EstimationResults` S7 object
#' @export
EstimationResults <- S7::new_class(
  name = "EstimationResults",
  package = "campsisfit",
  properties = list(
    model = S7::new_property(campsis_model_class, default = quote(campsismod::CampsisModel())),
    settings = EstimationSettings,
    fit = S7::class_any
  )
)

S7::method(print, EstimationResults) <- function(x, ...) {
  cat("Estimation method:", x@settings@method, "\n")
  objf <- x@fit$objf
  if (!is.null(objf)) {
    cat("Objective function value:", format(objf, nsmall = 3), "\n")
  }
  cat("\n")
  methods::show(x@model@parameters)
  return(invisible(x))
}

#' Convert an estimated (co)variance into the type of an OMEGA.
#'
#' Inverse of [campsismod::standardise()]: variances are converted to
#' `var`, `sd`, `cv` or `cv%`, covariances to `covar` or `cor`.
#'
#' @param omega OMEGA parameter
#' @param matrix estimated OMEGA matrix, with ETA names
#' @param eta1 name of the first ETA
#' @param eta2 name of the second ETA
#' @return numeric value
#' @keywords internal
omega_value <- function(omega, matrix, eta1, eta2) {
  if (eta1 == eta2) {
    var <- matrix[eta1, eta1]
    return(switch(
      omega@type,
      var = var,
      sd = sqrt(var),
      cv = sqrt(exp(var) - 1),
      "cv%" = 100 * sqrt(exp(var) - 1),
      stop(sprintf("Unsupported OMEGA type '%s' for %s", omega@type, eta1))
    ))
  }
  covar <- matrix[eta1, eta2]
  return(switch(
    omega@type,
    covar = covar,
    cor = covar / sqrt(matrix[eta1, eta1] * matrix[eta2, eta2]),
    stop(sprintf("Unsupported OMEGA type '%s' for %s/%s", omega@type, eta1, eta2))
  ))
}

#' Update the parameters of a Campsis model with estimated values.
#'
#' THETAs are matched by name (e.g. `THETA_CL`) and OMEGAs by ETA names (e.g.
#' `ETA_CL`). OMEGAs keep their type (e.g. `cv%`). Parameters absent from the
#' estimates (e.g. not used in the model code) and SIGMAs are left unchanged.
#'
#' @param model Campsis model
#' @param theta named vector of THETA estimates
#' @param omega estimated OMEGA matrix, with ETA names
#' @return the updated Campsis model
#' @keywords internal
update_model_parameters <- function(model, theta, omega) {
  parameters <- model@parameters
  isDiagOmega <- vapply(
    parameters@list,
    function(p) methods::is(p, "omega") && campsismod::is_diag(p),
    logical(1)
  )
  diagOmegas <- parameters@list[isDiagOmega]
  etaNames <- vapply(diagOmegas, campsismod::get_name_in_model, character(1))
  etaIndexes <- vapply(diagOmegas, function(p) p@index, integer(1))
  estimatedEtas <- rownames(omega)

  parameters@list <- lapply(parameters@list, function(p) {
    if (methods::is(p, "theta")) {
      name <- campsismod::get_name_in_model(p)
      if (name %in% names(theta)) {
        p@value <- unname(theta[[name]])
      }
    } else if (methods::is(p, "omega")) {
      eta1 <- etaNames[match(p@index, etaIndexes)]
      eta2 <- etaNames[match(p@index2, etaIndexes)]
      if (all(c(eta1, eta2) %in% estimatedEtas)) {
        p@value <- omega_value(p, matrix = omega, eta1 = eta1, eta2 = eta2)
      }
    }
    return(p)
  })
  model@parameters <- parameters
  return(model)
}
