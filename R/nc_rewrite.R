#' rewrite netcdf to fix attribute information
#'
#' @inheritParams ncwrite
#' @param bands variable names
#'
#' @examples
#' \dontrun{
#' nc_rewrite(file, outfile, bands, units, longnames, dates)
#' }
#' @export
nc_rewrite <- function(x, outfile,
                       bands = NULL, var.units = NULL, var.longname = NULL, dates = NULL, info = NULL) {
    if (is.character(x)) {
        file <- x
        lst  <- ncread_all(file)
        info <- ncdim_get(file)
    } else lst = x

    if (is.null(bands)) bands <- names(lst)
    if (!is.null(dates)) {
        dims <- ncdim_def_lonlat(info$lon, info$lat, dates)
    } else {
        dims <- info$dims
    }

    cat(sprintf("Writing into netcdf: %s\n", outfile))
    ncwrite(set_names(lst, bands), outfile, var.units, var.longname, dims = dims, compression = 1)
}
