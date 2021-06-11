
#' @export
ncdim_get <- function(file, sort.lat = TRUE) {
    fid <- nc_open(file)
    on.exit(nc_close(fid))

    dates <- nc_date(file) %>%
        as.Date.POSIXct() %>%
        {
            lubridate::make_date(year(.), month(.), 1)
        }
    lon <- ncread(file, "lon")
    # make sure increase order, this why gleam need to fix
    lat <- ncread(file, "lat")
    if (sort.lat) lat %<>% sort() # increasing order
    dims <- ncdim_def_lonlat(lon, lat, dates)

    unit <- purrr::map_chr(fid$var, "units")
    attrs <- ncatt_get(fid, 0) %>% unlist()

    listk(
        varnames = names(fid$var), unit,
        lon, lat, dates = dates,
        attrs, dims
    )
}
