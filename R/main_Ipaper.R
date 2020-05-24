rm_empty <- function (x) {
    if (is.list(x)) {
        x[sapply(x, length) > 0]
    }
    else {
        x[!is.na(x)]
    }
}
