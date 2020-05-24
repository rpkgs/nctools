#' Colored terminal output
#'
#' @param ... Strings to style.
#' @importFrom crayon green red bold underline
#'
#' @keywords internal
#' @rdname ok
#' @export
ok <- function(...) cat(green(...))

#' @rdname ok
#' @export
warn <- function(...) cat(red(...))
# warn  <- function(...) cat(red $ underline (...))
