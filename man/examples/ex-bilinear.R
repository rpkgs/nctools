\dontrun{
library(CMIP6tools)
set.seed(100)
range <- c(70, 140, 15, 55)

cellsize <- 2
lon <- seq(range[1], range[2], cellsize)
lat <- seq(range[3], range[4], cellsize)
nlat  <- length(lat); nlon <- length(lon)
ntime <- 10

# pnts <- expand.grid(lon, lat)
# pos  <- meshgrid(lon, lat)
# pnts2 <- list(pos$lon, pos$lat) %>% map(as.numeric) %>% as.data.table()
# (as.matrix(pnts2) - as.matrix(pnts) ) %>% summary()

grid <- list(lon = lon, lat = lat) #[,,1:100]
z <- array(rnorm(nlon*nlat*ntime), c(nlon, nlat, ntime)) 

r_bilinear  <- interp3d_bilinear(grid, z, range = range, cellsize_x = 1, convertTo2d = FALSE)
image(r_bilinear$data[, , 1])
}
