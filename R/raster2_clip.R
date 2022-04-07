#' clip raster2 regional data 
#' 
#' @param x raster2 object returned by `ncread_cmip`
#' 
#' @example R/examples/ex-raster2.R
#' @export
clip_raster2 <- function(x, range){
    lon <- x$grid$lon
    lat <- x$grid$lat
    
    range2 <- extend_range(range, lon, lat)
    loc <- meshgrid(lon, lat) %>% map(as.numeric)
    # x <- get_grid.raster2(x, varname = NULL)
    # loc <- coordinates(x) %>% set_names(c("lon", "lat"))
    loc <- data.table::as.data.table(loc) %>% cbind(I = 1:length(loc$lon))
    
    info <- loc[lon >= range2[1] & lon <= range2[2] & 
                lat >= range2[3] & lat <= range2[4], ]
    I_clip <- info$I

    varnames <- names(x) %>% setdiff(c("grid", "grid.origin")) %>% set_names(., .)
    clip_fun <- function(x) {
        if (is.matrix(x)) x[I_clip, ] else x[I_clip]  
    }

    data_lst <- foreach(varname = varnames) %do% {
        xi <- x[[varname]]
        if (varname == "HW") {
            res <- map(xi[1:6], function(xs_index) {
                map(xs_index, clip_fun) # for each probs
            })
            res$year <- xi$year               
        } else {
            res <- clip_fun(xi)
        }
        res
    }

    ## construct new raster2 ---------------------------------------------------
    grid <- x$grid
    lon_range <- range2[1:2]
    lat_range <- range2[3:4]
    lon <- inwhich(grid$lon, lon_range)
    lat <- inwhich(grid$lat, lat_range)
    
    x2 <- structure(
        c(list(grid = c(list(lon = lon, lat = lat, dim = c(length(lon), length(lat))), 
                x$grid.origin[c("cellsize_x", "cellsize_y")])), 
         data_lst),
        class = "raster2")
    x2
}

inwhich <- function(x, range) {
    x[x >= range[1] & x <= range[2]]
}

extend_range <- function(range, lon, lat) {
    # extend one pixel
    extend_range_i <- function(x, range) {
        min <- last(x[x < range[1]])
        max <- first(x[x > range[2]])
        
        min <- ifelse(length(min) == 0, range[1], min)
        max <- ifelse(length(max) == 0, range[2], max)
        c(min, max)
    }
    range2 <- c(extend_range_i(lon, range[1:2]), 
               extend_range_i(lat, range[3:4]))
    range2
}
