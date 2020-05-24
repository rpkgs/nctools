## code to prepare `DATASET` dataset goes here
library(lattice)

r <- readRDS("data-raw/HI_Threshold_MIROC-ESM.RDS")
# usethis::use_data(DATASET, overwrite = TRUE)

grid <- r$grid
lon <- grid$lon
lat <- grid$lat
date <- grid$date %>% as.Date()
dims <- ncdim_def_lonlat(lon, lat, date)

nlon = length(lon)
nlat = length(lat)

mean.T_degC = set_dim_raster2(r$mean.T_degC, nlon = nlon, nlat = nlat)
mean.T_HI   = set_dim_raster2(r$mean.T_HI, nlon = nlon, nlat = nlat)
TRS         = set_dim_raster2(r$TRS, nlon = nlon, nlat = nlat)

set_dim_raster2 <- function(x, nlon, nlat) {
    dim <- dim(x)
    ndim = pmax(dim[length(dim)], 1)
    set_dim(x, c(nlon, nlat, ndim))
}

probs <- c(0.9, 0.95, 0.975, 0.99, 0.995, 0.9975, 0.999, 0.9995, 0.99975, 0.9999)
probs = ncdim_def("prob", "Probability", probs)

levelplot(x[,,1])
# image(x)

