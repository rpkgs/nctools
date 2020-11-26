library(raster)
library(rgdal)
library(magrittr)
library(nctools)
library(matrixStats)
library(foreach)
library(iterators)
library(abind)
# library(sp2)

prj84 = CRS("+proj=longlat +datum=WGS84 +no_defs")
doy_date <- function(year, doy) as.Date(sprintf("%d%03d", year, doy), "%Y%j")
get_d8 <- function(doy) floor((doy-1)/8) + 1

files_Tmax = dir("N:/DATA/3h metrology data/daily_temp", "^Tmax*", full.names = TRUE)[22:40]
files_Tmin = dir("N:/DATA/3h metrology data/daily_temp", "^Tmin*", full.names = TRUE)[22:40]

ncdaily_tod8 <- function(files, outfile) {
    lst <- foreach(infile = files, i = icount()) %do% {
        runningId(i)
        r <- readGDAL(infile)
        mat = r@data %>% as.matrix()
        by  = get_d8(1:ncol(mat))
        ans = apply_row(mat, by)
    }

    vals = abind(lst, along = 2)
    grid = r[1]
    proj4string(grid) <- prj84
    grid@data <- as.data.frame(vals)
    writeGDAL(grid, outfile, options = c("COMPRESS=LZW"))
    # r_d8 = brick(grid)
    # writeRaster(r_d8, "ITCAS_forcing_Tmin.tif", overwrite=TRUE)
    grid
}

r_tmax = ncdaily_tod8(files_Tmax, "ITCAS_forcing_Tmax_gdal.tif")
r_tmin = ncdaily_tod8(files_Tmin, "ITCAS_forcing_Tmin_gdal.tif")


r <- brick("C:/Users/kongdd/Desktop/CLDAS/PRE18_20.tif")
writeRaster(r, "C:/Users/kongdd/Desktop/CLDAS/PRE18_20_v2.tif")
