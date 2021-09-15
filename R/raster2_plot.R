#' @importFrom sp CRS
get_prj <- function() {
    # prj84
    sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
}

#' plot raster2 object
#' 
#' @param x `cmip5` object returned from ncread
#' @param varname variable name in the data of `x`
#' @param zcols how many columns to plot? default is 4.
#' @param I_sel A vector indicating subset rows of `x`
#' @param fix_lon360 boolean. If true, it will convert longitude from 0-360 to 
#' -180 - 180.
#' @param is.SpatialMat is it spatial matrix (FALSE: numeric vector)?
#' @param ... other parameters to `spplot`
#' 
#' @return
#' * `plot`: `spplot` plot ojbect
#' * `grid`: SpatialGridDataframe
#' 
#' @importFrom sp spplot CRS coordinates proj4string SpatialPixelsDataFrame
#' @importFrom graphics plot
#' @export
plot.raster2 <- function(x, varname, zcols, I_sel, 
    range = NULL, 
    fix_lon360 = TRUE, 
    offset = 0, 
    sp.layout = NULL, 
    is.SpatialMat = FALSE,
    ...)
{
    if (missing(varname)) varname <- setdiff(names(x), c("grid", "grid.origin"))[1]
    if (is.null(range)) range <- c(range(x$grid$lon), range(x$grid$lat))

    if (fix_lon360 && range[2] > 180) {
        range <- c(180 - range[2], 180, range[3:4])
    }

    vals <- x[[varname]] + offset
    dim  <- dim(vals)
    n = 1    
    if (length(dim) <= 1) {
        vals_new = vals
    } else {
        n = pmin(last(dim), 4)
        vals_new = if (is.SpatialMat) 
            matrix(vals, ncol = 1) 
        else array_3dTo2d(vals)
    }

    df <- data.frame(vals_new)
    if (missing(zcols)) { zcols <- 1:n }
    
    grid <- get_grid.raster2(x, fix_lon360 = fix_lon360)
    grid@data <- df

    xlim <- range[1:2]
    ylim <- range[3:4]
    print(range)
    if (missing(I_sel)) I_sel <- 1:length(grid)
    
    p <- spplot(grid[I_sel, ], zcols, sp.layout = sp.layout, 
                ...,
           xlim = xlim, ylim = ylim,
           as.table = TRUE)
    p
    # grid
    # list(plot = p, grid = grid)
}

#' get_grid.raster2
#' 
#' @keywords internal
#' @inheritParams plot.raster2
#' @export
get_grid.raster2 <- function(x, varname = NULL, range = NULL, fix_lon360 = TRUE){
    grid <- get_grid.lonlat(x$grid$lon, x$grid$lat, fix_lon360)
    # grid2 <- get_grid(range, cellsize = cellsize, midgrid = FALSE)
    if (!is.null(varname)) {
        vals <- x[[varname]] %>% array_3dTo2d() %>% as.data.frame()
        if (!is.null(vals) && length(vals) > 0) {
            grid@data <- vals  
        }
    }
    grid
}

get_grid.lonlat <- function(lon, lat, fix_lon360 = FALSE, prj = NULL) {
    if (is.null(prj)) prj = get_prj()
    
    lon2 <- seq(min(lon), max(lon), length.out = length(lon))
    lat2 <- seq(min(lat), max(lat), length.out = length(lat))
    points <- expand.grid(lon2, lat2)
    grid <- SpatialPixelsDataFrame(points,
        data = data.frame(id = 1:nrow(points)),
        proj4string = prj84
    )
    if (fix_lon360) {
          grid %<>% fix_lon()
      }
    grid
}

fix_lon <- function (x) {
    pos <- coordinates(x)
    lon <- pos[, 1]
    I <- lon > 180
    cellsize_lon <- median(diff(sort(unique(lon))))
    lon_range <- pos[, 1] %>% range()
    delta1 <- 360 - diff(lon_range) - cellsize_lon
    pos[I, 1] <- pos[I, 1] - 360 + delta1
    x2 <- SpatialPixelsDataFrame(pos, data = x@data, proj4string = x@proj4string)
    x2
}
