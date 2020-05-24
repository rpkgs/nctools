#' @param r `raster2` object returned by [ncwrite()]
#' @param varname variable name in the data of `r`
#' @param ... other parameters to [ncwrite]
#' 
#' @rdname ncwrite
#' @export
ncwrite.raster2 <- function(r, file, varname = "array", ...){
    # only surport regular grids
    lons <- r$grid$lon
    lats <- r$grid$lat

    cellsize_x <- diff(lons[1:2])
    cellsize_y <- diff(lats[1:2])

    lon_range <- lons[c(1, length(lons))] + c(-1, 1)*cellsize_x/2
    lat_range <- lats[c(1, length(lats))] + c(-1, 1)*cellsize_y/2

    range <- c(lon_range, lat_range)

    lst_nc <- list(r$data) %>% set_names(varname)

    ncwrite(lst_nc, file, dates = r$grid$date, range = range, ...)
}
