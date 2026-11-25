#' @keywords internal
#' @importFrom methods getClass
#' @importFrom nlmixr2est nlmixr2
"_PACKAGE"

# Needed for the `@` operator on S7 objects with R < 4.3.0
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
