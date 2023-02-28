#' @keywords internal
#' @import magrittr foreach
#' @importFrom Ipaper apply_3d apply_col apply_row array_3dTo2d is_empty map
#' file_size fprintf clamp listk rm_empty str_year
#' @importFrom lubridate year
#' @importFrom methods is
#' @importFrom stats approx median setNames
#' @importFrom utils str
#' @importFrom stringr str_extract_all
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

first <- function(x) x[[1]]

last <- function(x) x[[length(x)]]

.onLoad <- function(libname, pkgname) {
    if (getRversion() >= "2.15.1") {
        utils::globalVariables(c(
            ".", "end_adj", "start_adj"
        ))
    }
}
