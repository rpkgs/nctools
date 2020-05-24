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
dim_prob = ncdim_def("prob", "Probability", probs)
dim_year = ncdim_def("year", "year", 1:200)

varnames = setdiff(names(r), c("grid", "grid.origin"))
dimname_last = c("prob", NA, NA)

for (i in seq_along(varnames)) {
    varname = varnames[i]
    dimnames = c("lon", "lat", dimname_last[i]) %>% rm_empty()
    dimnames %>% print()

    val <- r[[varname]] %>% spdata_array(nlon = nlon, nlat = nlat)
    print(str(val))
    # dim = dims[dimnames]
}

# dims = ncdim_def_lonlat(lon, lat, date)
# levelplot(x[,,1])
# image(x)

# missval = NA, untis = "", prec = "float", compression
# 2. define variables
    vars <- lapply(seq_along(varnames), function(i) {
        varname  = varnames[i];  
        # longname = var.longname[i]
        units = var.units[i]
        ndim <- length(dim(lst[[i]])) %>% pmax(1) #
        ncvar_def(varname, units, dims[1:ndim], missval, 
            # longname,
            prec=prec[i], compression = compression)
    })

    # put variables into fid
    if (!file.exists(file) || overwrite) {
        if (file.exists(file) && overwrite) file.remove(file)
        fid <- nc_create(file, vars)
    } else {
        for(var in vars) {
            # if not exists, add new
            if (!(var$name %in% names(fid$var))) {
                fid <- ncvar_add( fid, var )
            }
        }
    }
    on.exit(nc_close(fid))

    # write values
    ncwrite_var(lst, fid, vars, prec, scale, offset)
