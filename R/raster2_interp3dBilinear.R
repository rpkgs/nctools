#' Bilinear interpolation for 3d array
#' 
#' @param grid A list object with "lon", "lat".
#' \itemize{
#' \item lon numeric, with the length of nlon. 
#' \item lat numeric, with the length of nlat.
#' \item ... other elements will be ignore
#' }
#' @param z 3d array (lon, lat, time), with the dimension of `[nlon, nlat, ntime]`.
#' `image(z[,,1])` should look normal.
#' 
#' @param range A numeric vector, `[lat_min, lat_max, lon_min, lon_max]`
#' @param cellsize_x Degree, cell size in the horizontal direction.
#' @param cellsize_y Degree, cell size in the Vertical direction.
#' @param na.rm If true, Na value in margin will be fixed.
#' 
#' @rdname interp3d_bilinear
#' 
#' @note High resolution interpolating to low resolution will lead to unreliable result, if 
#' `cellsize.new/cellsize.origin > 2`.
#' 
#' @example man/examples/ex-raster2.R
#' @export 
interp3d_bilinear <- function (grid, z, 
    range = c(70, 140, 15, 55), 
    cellsize_x = 1, cellsize_y = cellsize_x, 
    na.rm = FALSE, 
    convertTo2d = TRUE) 
{
    lon_range <- range[1:2]
    lat_range <- range[3:4]
    
    dx <- cellsize_x/2
    dy <- cellsize_y/2
    xx <- seq(lon_range[1] + dx, lon_range[2], by = cellsize_x)
    yy <- seq(lat_range[1] + dy, lat_range[2], by = cellsize_y)
    nxx <- length(xx)
    nyy <- length(yy)
    
    # Due to longitude is the first dimension, there is a `transform`.
    loc <- meshgrid(xx, yy) %>% set_names(c("x", "y"))

    grid.origin <- grid
    grid$lon <- xx; grid$lat <- yy # update dim
    grid$cellsize_x <- cellsize_x
    grid$cellsize_y <- cellsize_y
    # dim <- list(lon = xx, lat = yy) # , date = grid.origin$date

    x <- grid.origin$lon
    y <- grid.origin$lat
    # z <- obj.value # 3d array
    
    nx <- length(x)
    ny <- length(y)
    
    ndim <- length(dim(z))
    nz   <- ifelse(ndim <= 1, 1, last(dim(z)))

    if (ndim >= 3 || ndim <= 1) {
        z <- `dim<-`(z, c(nx*ny, nz))
    }
    
    lx  <- approx(x, 1:nx, loc$x, rule = 2)$y
    ly  <- approx(y, 1:ny, loc$y, rule = 2)$y
    lx1 <- floor(lx)
    ly1 <- floor(ly)

    ex <- lx - lx1
    ey <- ly - ly1
    ex[lx1 == nx] <- 1
    ey[ly1 == ny] <- 1
    lx1[lx1 == nx] <- nx - 1
    ly1[ly1 == ny] <- ny - 1

    I_fix <- array(1:(nxx*nyy), dim = c(nxx, nyy)) %>% .[, nyy:1] %>% as.numeric()
    pos   <- array(1:(nx*ny), dim = c(nx, ny))
    # if (matY.descend){ pos %<>% flipud() } 
    
    # if na value exist, exchange value in vertical direction as `raster` package
    v00 <- z[pos[cbind(lx1    , ly1    )], , drop = FALSE] # left, bottom
    v01 <- z[pos[cbind(lx1    , ly1 + 1)], , drop = FALSE]
    v10 <- z[pos[cbind(lx1 + 1, ly1    )], , drop = FALSE]
    v11 <- z[pos[cbind(lx1 + 1, ly1 + 1)], , drop = FALSE]

    if (na.rm == TRUE) {
        vv    <- abind(v00, v01, v10, v11, along = 3)
        vmean <- apply_3d(vv, 3, rowMeans2)

        # browser()
        I_good <- rowSums(is.na(vmean)) != ncol(vv) # if all na, no need to interp
        # 859 NA values, 
        fix_na_each(v00, v01)
        fix_na_each(v11, v10)
        
        fix_na(v00, vmean)
        fix_na(v01, vmean)
        fix_na(v10, vmean)
        fix_na(v11, vmean)
    }
    
    # fix NA values in z
    # vals <- abind(v00 * (1 - ex) * (1 - ey), 
    #         v10 * ex * (1 - ey),
    #         v01 * (1 - ex) * ey,
    #         v11 * ex * ey, along = 3) %>% 
    #     apply_3d(3, rowMeans2) %>% 
    #     multiply_by(4)
    vals <- v00 * (1 - ex) * (1 - ey) + 
            v10 * ex * (1 - ey) + 
            v01 * (1 - ex) * ey + 
            v11 * ex * ey

    # if (matY.descend) vals <- vals[I_fix, ]
    if (!convertTo2d) { vals <- `dim<-`(vals, c(length(xx), length(yy), nz)) }
    structure(list(grid.origin = grid.origin, 
            grid = grid, 
            data = vals),
        class = "raster2")
}

# ' @examples
# ' x <- 1:4; x[3] <- NA
# ' y <- 1:4
# ' fix_na(x, y)
fix_na <- function(value, new){
    name_old <- substitute(value)
    name_new <- substitute(new)
    # I <- is.na(x) #& I_good            
    eval(substitute(x[is.na(x)] <- y[is.na(x)], 
        list(x = name_old, y = name_new)), envir = parent.frame())
}

fix_na_each <- function(value, new){
    name_old <- substitute(value)
    name_new <- substitute(new)
    # I <- is.na(x) #& I_good            
    eval(substitute(x[is.na(x)] <- y[is.na(x)], 
        list(x = name_old, y = name_new)), envir = parent.frame())
    eval(substitute(x[is.na(x)] <- y[is.na(x)], 
        list(x = name_new, y = name_old)), envir = parent.frame())
}


# #' @rdname interp3d_bilinear
# interp3d.grid_akima <- function(obj, 
#                           range = c(15, 55, 70, 140), 
#                           cellsize_x = 1, cellsize_y = cellsize_x)
# {
#     nslice <- dim(obj$z)[3]
    
#     lat_range <- range[1:2]
#     lon_range <- range[3:4]
    
#     res <- llply(1:nslice, function(i){
#         bilinear.grid(x = obj$x, y = obj$y, z = obj$z[,,i],
#                       xlim = lon_range, ylim = lat_range, dx = cellsize_x, dy = cellsize_y)$z
#     }, .progress = "text") %>% abind(along = 3)
#     res
# }

# #' @rdname interp3d_bilinear
# interp3d.grid_fields <- function(obj, range, cellsize_x = 1, cellsize_y = cellsize_x){
#     nslice <- dim(obj$z)[3]
    
#     lat_range <- range[1:2]
#     lon_range <- range[3:4]
    
#     x <- seq(lon_range[1], lon_range[2], by = cellsize_x)
#     y <- seq(lat_range[1], lat_range[2], by = cellsize_y)
    
#     res <- llply(1:nslice, function(i){
#         interp.surface.grid(list(x = obj$x, y = obj$y, z = obj$z[,,i]), 
#                             grid.list = list(x = x, y = y))$z
#     }, .progress = "text") %>% abind(along = 3)
#     res
# }


# # test the performance of interp3d_bilinear
# main <- function(){
#   library(akima)
#   library(abind)
#   library(fields)
#   
#     file <- "F:/Rcmip5/left/historical_tasmax/tasmax_day_HadCM3_historical_r10i1p1_19341201-19591130.nc"
#     l <- read_nc(file)

#     ## 1. akima 
#     obj <- list(x = l$dims$lon, y = l$dims$lat, z = l$var.value[,,1:100]) #[,,1:100]

#     range <- c(15, 55, 70,140)
#     range <- c(-90, 90, 0, 360)

#     rbenchmark::benchmark(
#         res_fields <- interp3d.grid_fields(obj, range = range, cellsize_x = 1),
#         # res_akima  <- interp3d.grid_akima(obj, range = range, cellsize_x = 1),
#         res_bilinear  <- interp3d_bilinear(obj, range = range, cellsize_x = 1),
#         replications = 1
#     )
#     all.equal(res_fields, res_akima)
# }

# loc <- meshgrid(x = 70:140, y = 15:55) %>% set_names(c("x", "y"))
# grid.list <- list(x = 70:140, y = 15:55)
# y <- interp.surface.grid3d(obj, grid.list)$z

# image(y)
# r <- raster(y$z, 70, 140, 15, 55)
