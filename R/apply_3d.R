#' strengthened apply function for 3d array
#'
#' @param array 3d or 2d array
#' @param dim The dim to apply function, if `dim > ndim(array)`, dim will be reset to
#' `ndim(array)`.
#'
#' @export
apply_3d <- function(array, dim = 3, FUN = rowMeans2, by = NULL, na.rm = TRUE, ...) {
    # TODO: add by at here
    dims <- dim(array)
    ndim <- length(dims) # dimensions
    if (dim > ndim) dim = ndim

    I_dims     <- setdiff(1:ndim, dim) # dimensions order
    dims_head  <- dims[I_dims]         # header dimensions

    # move grouped dim to the last
    if (dim != ndim){
        array %<>% aperm(c(I_dims, dim))
    }
    mat <- array_3dTo2d(array)

    if (is.null(by)) {
        ans <- FUN(mat, na.rm = na.rm, ...)
        dim_new <- dims_head
    } else {
        dim_new <- c(dims_head, length(unique(by)))
        ans <- apply_row(mat, by, FUN, na.rm = na.rm, ...)
    }
    dim(ans) <- dim_new
    ans
}

array_3dTo2d <- function (array, I_grid)
{
    dim <- dim(array)
    if (length(dim) >= 3) {
        dim(array) <- c(prod(dim[1:2]), dim[3])
    }
    if (!missing(I_grid)) {
        array <- array[I_grid, ]
    }
    return(array)
}

#' @export
#' @rdname apply_3d
apply_row <- function(mat, by, FUN = rowMeans2, w = NULL, na.rm = TRUE, ...) {
    if (length(by) != ncol(mat)) {
        stop("Length of by is not equal to ncol of mat")
    }
    grps <- unique(by) %>% sort() %>% set_names(., .)

    lapply(grps, function(grp) {
        I <- which(by == grp)
        FUN(mat[, I], na.rm = na.rm, w = w[I], ...)
    }) %>% do.call(cbind, .) %>%
        set_rownames(rownames(mat))
}
