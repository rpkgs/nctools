#' @export
meshgrid <- function(x, y = x) 
{
  if (!is.numeric(x) || !is.numeric(y)) 
    stop("Arguments 'x' and 'y' must be numeric vectors.")
  x <- c(x)
  y <- c(y)
  n <- length(x)
  m <- length(y)
  X <- matrix(rep(x, each = m), nrow = m, ncol = n)
  Y <- matrix(rep(y, times = n), nrow = m, ncol = n)
  return(list(X = X, Y = Y))
}

#' coords_from_range
#' 
#' @export
coords_from_range <- function(range = c(-180, 180, -60, 90), cellsize = 1) {
  lon = seq(range[1] + cellsize/2, range[2], cellsize)
  lat = seq(range[3] + cellsize/2, range[4], cellsize)
  # loc = meshgrid(lon, lat)
  loc = expand.grid(lon = lon, lat = lat)
  listk(lon, lat, loc)
}
