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
    if (missing(dimnames_last)) dimnames_last = NULL

    grid <- r$grid
    calendar <- r$grid.origin$calendar
    if (is.null(calendar)) calendar = "gregorian"

    lon  <- grid$lon
    lat  <- grid$lat
    date <- grid$date
    dims <- ncdim_def_lonlat(lon, lat, date)
    year <- r$grid$year

    probs <- c(0.9, 0.95, 0.975, 0.99, 0.995, 0.9975, 0.999, 0.9995, 0.99975, 0.9999)

    dim_prob = ncdim_def("prob", "Probability", probs)
    dim_year = if (is.null(year)) NULL else ncdim_def("year", "year", year)

    dims0 = ncdim_def_lonlat(lon, lat, prob = dim_prob, year = dim_year)
    dims %<>% c(dims0)

    nlon = length(lon)
    nlat = length(lat)

    varnames = setdiff(names(r), c("grid", "grid.origin"))
    lst <- lapply(r[varnames], function(.x) spdata_array(.x, nlon = nlon, nlat = nlat))

    dims_last <- sapply(lst, function(x) dim(x)[3])
    dimnames_last = c(dimnames_last, rep(NA, length(lst) - length(dimnames_last)))
    dimnames_last[dims_last == 1] = NA
    dimnames_last[varnames == "TRS"] = "prob"
    dimnames_last[dims_last == length(date)]  = "time"
    dimnames_last[dims_last == length(year)]  = "year"

    I_year = grep("year", varnames)
    dimnames_last[I_year] = "year"

    ncwrite(lst, file,
        dims = dims,
        dimnames_last = dimnames_last,
        overwrite = overwrite
    )
}
