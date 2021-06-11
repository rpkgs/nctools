listk <- function (...) {
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) 
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0) 
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

#' @export
str_year <- function(x) stringr::str_extract(basename(x), "\\d{4}")

rm_empty <- function(x) {
    if (is.list(x)) {
        x[sapply(x, length) > 0]
    }
    else {
        x[!is.na(x)]
    }
}
