#_______________________________________________________________________________
#----                               JSON                                    ----
#_______________________________________________________________________________

#' Convert an object to JSON.
#'
#' @param object object to convert, e.g. [EstimationSettings()]
#' @param ... extra arguments, not used
#' @return JSON string
#' @export
#' @examples
#' cat(to_json(EstimationSettings(method = "saem", control = list(nBurn = 200))))
to_json <- S7::new_generic("to_json", dispatch_args = "object")

S7::method(to_json, EstimationSettings) <- function(object, ...) {
  control <- object@control
  if (length(control) == 0) {
    control <- structure(list(), names = character(0)) # serialised as {} instead of []
  }
  json <- list(method = object@method, control = control)
  return(as.character(jsonlite::toJSON(json, auto_unbox = TRUE, pretty = TRUE, digits = NA)))
}

get_settings_schema <- function() {
  return(system.file("extdata", "campsisfit_settings.schema.json", package = "campsisfit"))
}

#' Read estimation settings from JSON.
#'
#' The JSON content is validated against the schema shipped with the package
#' (`inst/extdata/campsisfit_settings.schema.json`).
#'
#' @param json path to a JSON file or JSON content in string form
#' @return an [EstimationSettings()] object
#' @export
#' @examples
#' read_settings(system.file("extdata", "settings_focei.json", package = "campsisfit"))
#' read_settings('{"method": "saem", "control": {"nBurn": 200, "nEm": 300}}')
read_settings <- function(json) {
  if (!is.character(json) || length(json) != 1) {
    stop("Argument json must be a path to a JSON file or the JSON string")
  }
  if (grepl(pattern = "^\\s*\\{", x = json)) {
    rawJson <- json
  } else {
    if (!file.exists(json)) {
      stop(sprintf("File '%s' does not exist", json))
    }
    rawJson <- paste0(readLines(json, warn = FALSE), collapse = "\n")
  }
  validator <- jsonvalidate::json_schema$new(get_settings_schema())
  validator$validate(rawJson, error = TRUE)

  data <- jsonlite::parse_json(rawJson, simplifyVector = TRUE)
  control <- data$control
  if (length(control) == 0) {
    control <- list()
  }
  return(EstimationSettings(method = data$method, control = control))
}

#' Write estimation settings to a JSON file.
#'
#' @param settings an [EstimationSettings()] object
#' @param file path to the JSON file
#' @return the file path, invisibly
#' @export
#' @examples
#' file <- tempfile(fileext = ".json")
#' write_settings(EstimationSettings(method = "foce"), file)
#' read_settings(file)
write_settings <- function(settings, file) {
  if (!S7::S7_inherits(settings, EstimationSettings)) {
    stop("Argument settings must be an EstimationSettings object")
  }
  writeLines(to_json(settings), con = file)
  return(invisible(file))
}
