#_______________________________________________________________________________
#----                          Estimation settings                          ----
#_______________________________________________________________________________

#' Get the estimation methods supported by campsisfit.
#'
#' Each method is forwarded as is to the `est` argument of
#' [nlmixr2est::nlmixr2()].
#'
#' @return character vector
#' @export
#' @examples
#' supported_methods()
supported_methods <- function() {
  return(c("focei", "foce", "foi", "fo", "laplace", "saem", "posthoc"))
}

#' Estimation settings.
#'
#' Settings used by [estimate()].
#'
#' @param method estimation method, one of [supported_methods()]:
#'   `"focei"` (default), `"foce"`, `"foi"`, `"fo"`, `"laplace"`, `"saem"` or
#'   `"posthoc"`
#' @param control named list of options passed to the nlmixr2 control function
#'   of the chosen method, e.g. [nlmixr2est::foceiControl()] for `"focei"` or
#'   [nlmixr2est::saemControl()] for `"saem"`. Values must be atomic so that
#'   the settings can be exported to JSON. Empty by default (nlmixr2 defaults).
#' @return an `EstimationSettings` S7 object
#' @export
#' @examples
#' EstimationSettings()
#' EstimationSettings(method = "saem", control = list(nBurn = 200, nEm = 300, print = 0))
EstimationSettings <- S7::new_class(
  name = "EstimationSettings",
  package = "campsisfit",
  properties = list(
    method = S7::new_property(S7::class_character, default = "focei"),
    control = S7::new_property(S7::class_list, default = list())
  ),
  validator = function(self) {
    method <- self@method
    if (length(method) != 1 || is.na(method)) {
      return("@method must be a single character string")
    }
    if (!method %in% supported_methods()) {
      return(sprintf(
        "@method must be one of %s, not '%s'",
        paste0("'", supported_methods(), "'", collapse = ", "),
        method
      ))
    }
    control <- self@control
    if (length(control) > 0) {
      controlNames <- names(control)
      if (is.null(controlNames) || any(controlNames == "") || anyDuplicated(controlNames) > 0) {
        return("@control must be a list with unique, non-empty names")
      }
      if (!all(vapply(control, is.atomic, logical(1)))) {
        return("@control values must be atomic (character, numeric or logical)")
      }
    }
    return(NULL)
  }
)
