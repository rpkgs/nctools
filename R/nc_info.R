#' ncinfo
#' @export
ncinfo <- function(file, verbose = TRUE) {
    fid <- nc_open(file)
    on.exit(nc_close(fid)) # make sure closed on error

    if (verbose) print(fid)
    names(fid$var) %>% set_names(., .)
}

#' @rdname ncinfo
#' @export 
nc_info <- ncinfo

#' @export 
raster_info <- function(file) {
    subfix = stringr::str_extract(basename(file), "(?<=\\.).{2,4}$")

    if (subfix == "nc"){
        # nc file
        ncdim_get(file)
    } else {
        # raster file
        suppressWarnings(x <- rgdal::GDALinfo(file))
        lon = x %>% {seq(.["ll.x"] + .['res.x']/2, by = .['res.x'], length.out = .['columns'])}
        lat = x %>% {seq(.["ll.y"] + .['res.y']/2, by = .['res.y'], length.out = .['rows'])}
        listk(lon, lat)
        # stop("unsupport file type!")
    }
}
