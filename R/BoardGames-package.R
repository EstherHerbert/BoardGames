#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

# Options ----------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  options(bggAnalytics.token = 'a323e76c-eb09-4549-b596-2e0976ce48bd')
}
