#' @param r `raster2` object returned by [ncwrite()]
#' @param varname variable name in the data of `r`
#' @param ... other parameters to [ncwrite]
#' 
#' @rdname ncwrite
#' @export
ncwrite_raster2 <- function(r, file, dims = NULL, dimnames_last, 
    overwrite = TRUE, ...)
{
    # This function is tested at Threshold.
    if (missing(dimnames_last)) dimnames_last = c("prob", NA, NA)

    grid <- r$grid
    lon  <- grid$lon
    lat  <- grid$lat
    date <- grid$date %>% as.Date()
    dims <- ncdim_def_lonlat(lon, lat, date)

    probs <- c(0.9, 0.95, 0.975, 0.99, 0.995, 0.9975, 0.999, 0.9995, 0.99975, 0.9999)
    dim_prob = ncdim_def("prob", "Probability", probs)
    dim_year = ncdim_def("year", "year", 1:200)
    dims0 = ncdim_def_lonlat(lon, lat, prob = dim_prob, year = dim_year)
    dims %<>% c(dims0) 
    
    nlon = length(lon)
    nlat = length(lat)

    varnames = setdiff(names(r), c("grid", "grid.origin"))
    lst <- lapply(r[varnames], function(.x) spdata_array(.x, nlon = nlon, nlat = nlat))
    
    ncwrite(lst, file,
        dims = dims,
        dimnames_last = dimnames_last,
        overwrite = overwrite
    )
}
