#! /usr/bin/Rscript --no-init-file
# Dongdong Kong ----------------------------------------------------------------
# Copyright (c) 2021 Dongdong Kong. All rights reserved.
# source('scripts/main_pkgs.R')
# library(raster)
# library(glue)
library(nctools)
library(glue)
library(magrittr)
# library(lubridate)
bandNames <- function(file) names(raster::brick(file))

bands_all <- c("GPP", "Ec", "Es", "Ei", "ET_water", "ET_pot", "ET", "days_coverage")
var.units <- c("gC m^2-1 y-1", "mm/y", "mm/y", "mm/y", "mm/y", "mm/y", "mm/y", "days") %>% set_names(bands_all)
# "PET"  "Ec"   "Es"   "Ei"   "Prcp" "Tair" "Rl"   "Rs"
var.longname <- c(
    "vegetation gross primary production",
    "canopy transpiration", "soil evaporation", "canopy interception",
    "water surface evaporation", "penman potential evapotranspiration", "Actual Evapotranspiration", ""
) %>% set_names(bands_all)

# infile = "GLDAS_V021_yearly_2000-01-02.tif"
# var.units = c("gC m^2-1 y-1", "mm/y", "mm/y", "mm/y", "mm/y", "mm/y")
# # "PET"  "Ec"   "Es"   "Ei"   "Prcp" "Tair" "Rl"   "Rs"
# var.longname = c("vegetation gross primary production",
#     "canopy transpiration", "soil evaporation", "canopy interception",
#     "water surface evaporation", "penman potential evapotranspiration")

if (0) {
    indir = "v017"
    subfixs = c("CFSV2", "ERA5L", "GLDASv21")
    for (subfix in subfixs) {
        pattern = glue("*{subfix}.*.tif")
        outfile = glue("PMLV2_v017_G010_yearly_{subfix}_2000-2020.nc")
        files <- dir(indir, pattern, full.names = TRUE)
        bands <- bandNames(files[1])

        tryCatch({
            tiff2nc_process(files, outfile, var.units[bands], var.longname[bands], run = TRUE)
        }, error = function(e) {
            message(sprintf('%s', e$message))
        })
    }
}

## previous version
indir = "/mnt/h/global_WB/ET/diagnosed_PML/PMLV2_v016"
subfixs = c("v014", "v016")[2]
subfix  = subfixs[1]
for (subfix in subfixs) {
    pattern = glue("*{subfix}.*.tif")
    files <- dir(indir, pattern, full.names = TRUE)
    outfile = glue("{indir}/../PMLV2_{subfix}_G010_yearly_GLDASv21_2003-2020.nc")
    outfile_temp = glue("{indir}/../PMLV2_{subfix}_G010_yearly_GLDASv21_2003-2020_temp.nc")
    bands <- bandNames(files[1])
    tryCatch({
        tiff2nc_process(files, outfile, var.units[bands], var.longname[bands], run = TRUE)
    }, error = function(e) {
        message(sprintf('%s', e$message))
    })
}

tiff2nc(files)
tiff2nc_merge(files, outfile_temp)
