#' Convert Spatial* data into 3d array
#'
#' @param vals `[ngrid, ntime]`
#' If ndim <= 2, range and cellsize is necessary.
#' @param range A numeric vector, `[lon_min, lon_max, lat_min, lat_max]`
#' @param cellsize double, cell size
#' @param nlon,nlat If nlon and nlat provided, range and cellsize will be ignored
#'
#' @keywords internal
#' @examples
#' #
#' @export
spdata_array <- function(vals,
    range = NULL, cellsize = 0.1,
    nlon = NULL, nlat = NULL,
    flip = FALSE, to2d = FALSE)
{
    dim <- dim(vals)
    ndim <- length(dim)

    isnon_lonlat = is.null(nlon) && is.null(nlat)
    if (isnon_lonlat && !is.null(range)) {
        lon_range <- range[1:2]
        lat_range <- range[3:4]
        nlon <- diff(lon_range) / cellsize
        nlat <- diff(lat_range) / cellsize
    }

    ntime = 1
    if (ndim <= 1) {
        # vals <- vals#if (isnon_lonlat) as.matrix(vals) else matrix(vals, nlon, nlat)
    } else if (ndim >= 2) {
        if (isnon_lonlat && is.null(range)) {
            nlon = dim[1]
            nlat = dim[2]
        }
        ntime = if (ndim == 2 && is.null(range) && isnon_lonlat) 1 else dim[ndim]
        vals <- set_dim(vals, c(nlon*nlat, ntime))
    }
    if (flip) {
        id <- matrix(1:prod(nlon, nlat), nlon, nlat) %>% flipud() %>% as.numeric()
        vals <- vals[id, , drop = FALSE]
    }
    dimnew = if (to2d) c(nlon * nlat, ntime) else c(nlon, nlat, ntime)
    set_dim(vals, dimnew)
}


set_dim <- function(x, dim) {
    dim(x) <- dim
    x
}

# from Ipaper
flipud <- function(x, ...) {
    I = ncol(x):1
    ndim <- length(dim(x))
    if (ndim == 2) {
        x[, I]
    } else if (ndim == 3) {
        x[,I,]
    }
}

repmat <- function (a, n, m = n)
{
    if (length(a) == 0)
        return(c())
    if (!is.numeric(a) && !is.complex(a))
        stop("Argument 'a' must be a numeric or complex.")
    if (is.vector(a))
        a <- matrix(a, nrow = 1, ncol = length(a))
    if (!is.numeric(n) || !is.numeric(m) || length(n) != 1 ||
        length(m) != 1)
        stop("Arguments 'n' and 'm' must be single integers.")
    n <- max(floor(n), 0)
    m <- max(floor(m), 0)
    if (n <= 0 || m <= 0)
        return(matrix(0, nrow = n, ncol = m))
    matrix(1, n, m) %x% a
}
