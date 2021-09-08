
#' @export
ncdim_get <- function(file, sort.lat = TRUE) {
    fid <- nc_open(file)
    on.exit(nc_close(fid))

    dates <- nc_date(file)
    
    tryCatch({
        lon <<- ncread(file, "lon")
        lat <<- ncread(file, "lat")
    }, error = function(e) {
        message(sprintf("%s", e$message))
        lon <<- ncread(file, "longitude")
        lat <<- ncread(file, "latitude")
    })
    # make sure increase order, this why gleam need to fix
    if (sort.lat) lat %<>% sort() # increasing order
    dims <- ncdim_def_lonlat(lon, lat, dates)

    unit <- purrr::map_chr(fid$var, "units")
    longname <- purrr::map_chr(fid$var, "longname")
    attrs <- ncatt_get(fid, 0) %>% unlist()
    
    listk(
        varnames = names(fid$var), unit, longname, 
        lon, lat, dates = dates,
        attrs, dims
    )
}
