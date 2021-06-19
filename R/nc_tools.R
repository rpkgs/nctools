merge_nc <- function(files) {
    # all files should have the same format and same variables
}

#' @export
nc_range <- function(r) {
    if (class(r)[1] != "raster2") {
        r <- ncread(r, NULL, ntime = -1)
    }

    cellsize_x = r$grid.origin$cellsize_x
    cellsize_y = r$grid.origin$cellsize_y
    lat        = r$grid$lat
    lon        = r$grid$lon
    lat_range  = range(lat) + c(-1, 1)*cellsize_y/2
    lon_range  = range(lon) + c(-1, 1)*cellsize_y/2

    range    = c(lon_range, lat_range)
    cellsize = c(cellsize_x, cellsize_y)
    listk(range, cellsize)
}

#' Aggregate to yearly scale
#'
#' @param infile,outfile INPUT and OUTPUT nc files
#' @param vars_mean variables which are aggregated by `mean`. The others will be
#' `sum`.
#'
#' @note Current function can't handle with NA vlaues.
#'
#' @importFrom lubridate make_date
#' @export
nc_aggregateToYearly <- function(infile, outfile, vars_mean = NULL, overwrite = FALSE) {
    varnames <- nc_info(infile, FALSE)
    dates <- nc_date(infile, to_char = FALSE)

    years        = year(dates) %>% unique() %>% sort()
    dates_yearly = make_date(years, 1, 1)

    range <- nc_range(infile)$range
    # type = "sum"
    # ans <- aggregate_yearly(l$data[[1]], dates)
    for (varname in varnames) {
        # print(varname)
        fprintf("process: %s ...\n", varname)
        l <- ncread(infile, varname, ntime = -1, grid_type = NULL) # vec
        array <- l$data[[1]]

        type <- if (!is.null(vars_mean) && varname %in% vars_mean) "mean" else "sum"
        vals <- aggregate_yearly(array, dates, type = type)

        ncwrite(set_names(list(vals), varname), outfile,
            range = range,
            # var.units, missval = missval,
            dates = dates_yearly,
            verbose = FALSE, overwrite = overwrite
        )
    }
}
