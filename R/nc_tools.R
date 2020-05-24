merge_nc <- function(files) {
    # all files should have the same format and same variables
}

#' @export
nc_info <- function(file, verbose = TRUE) {
    fid <- nc_open(file)
    on.exit(nc_close(fid)) # make sure closed on error

    if (verbose) print(fid)
    names(fid$var) %>% set_names(., .)
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

#' nc_date
#'
#' @inheritParams ncread
#' @param fid An object of class ncdf4 retuned by [ncdf4::nc_open()]
#' @param datastr Boolean. Whether convert date to string?
#'
#' @keywords internal
#'
#' @importFrom stringr str_extract
#' @import PCICt
#' @export
nc_date <- function(fid, ntime = -1, to_char = FALSE){
    if (class(fid)[1] != "ncdf4") {
        fid <- nc_open(fid)
        on.exit(nc_close(fid))
    }

    ctime    <- fid$dim$time # time class
    origin   <- ctime$units %>% str_extract("\\d{2,4}-\\d{1,2}-\\d{1,2}")
    calendar <- ctime$calendar

    nslice <- fid$dim$time$len
    if (ntime == -1) ntime <- nslice

    date <- {as.PCICt(origin, cal=calendar) + (fid$dim$time$vals[1:ntime])*86400} %>% format(DATE_FORMAT)
    if (!to_char) date %<>% as.Date()
    date
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
