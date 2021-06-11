#' read nc file
#'
#' `getCMIP5date` make sure that if dates geenrated according to
#' `time` and dates in files consistent. If not, error will occur.
#'
#' @param file Character, nc file name.
#' @param varnames String or integer vector which specify which variable to read.
#' if integer vector, for example: `1` means the first variable to read;
#' - `0` or `NULL`: no data to be read, just return dimension infomation
#' - `-1`         : read all data
#' @param range A numeric vector, `[lon_min, lon_max, lat_min, lat_max]`. If `range`
#' is `NULL`, `delta` will be ignored, and all data will be read.
#' @param ntime How many time to read?
#' @param DatePeriod Date \strong{character} vector, starting and ending date.
#' `DatePeriod` works only in the situation when `time2date` is true.
#' Note that in that situation if `DatePeriod` provided, ntime will be overrided.
#' @param delta suggest 3deg. Extend range into `[lat_min - delta, lat_max + delta,
#' lon_min - delta, lon_max + delta]`. This is to make sure no missing value in
#' margin.
#' @param grid_type 'vec', 'mat' or NULL. If not null, 3d array will be converted
#' to 2d matrix, with the dimension of `[nlon*nlat, ntime]`.
#' - `vec`: spdata_array(vals, flip = TRUE) will be applied.
#' - `mat`: spdata_array(vals, flip = FALSE)
#' @param scale,offset If the variable in the netCDF file has a scale and/or offset
#' attribute defined, the returned data are automatically and silently scaled
#' and/or offset as requested. Then you should just leave scale = 1,
#' and offset = 0.
#' @param value_range If not null, values out of `value_range` are set to
#' NA.
#' @param adjust_lon If true, lon = lon + cellsize_y/2
#' @param check_date If true, check whether date in nc file is consistent with
#' `date_begin` and `date_end` in its filename.
#'
#' @export
#' @importFrom lubridate date day
#'
#' @note
#' In some CMIP5 file, `time` band is incorrect. So we decide to generate
#' date by the date information containing in the file name.
#'
#' This function is initially designed for CMIP5 nc files.
#' @seealso spdata_array
#'
#' @examples
#' \dontrun{
#' file <- "H:/CMIP5/historical_tasmax/tasmax_day_ACCESS1-0_historical_r1i1p1_20000101-20051231.nc"
#'
#' range <- c(0, 360, -90, 90)
#' range <- c(70,140, 15, 55)
#' l <- ncread(file, varnames = "tasmax", range = range, delta = 2, ntime = 1)
#' }
#' 
#' @return list(dim, date, value)
ncread <- function(file,
                    varnames = 1L,
                    range = NULL,
                    delta = 0,
                    ntime = -1,
                    DatePeriod,
                    grid_type = NULL,
                    scale = 1,
                    offset = 0,
                    value_range = NULL,
                    adjust_lon = FALSE,
                    check_date = FALSE)
{
    fid  <- nc_open(file, readunlim = !check_date)
    on.exit(nc_close(fid)) # make sure closed on error

    if (is.numeric(varnames)) {
        varId <- as.integer(varnames)
        if (length(varId) == 1) {
            if (varId == 0 || varId <= -2) {
                varnames <- NULL
            } else if (varId == -1) {
                varnames <- names(fid$var)
            } else {
                varnames <- names(fid$var)[varId]
            }
        } else if (length(varId) == 0) {
            varnames <- NULL
        } else {
            varnames <- names(fid$var)[varId]
        }
    }

    lon <- fid$dim$lon$vals
    lat <- fid$dim$lat$vals

    cellsize_x <- lon[2] - lon[1]
    cellsize_y <- lat[2] - lat[1]

    if (adjust_lon) lon <- lon + cellsize_x/2 # longitude begin from 0

    # time variable not work for some stupid model!
    ctime    <- fid$dim$time # time class
    origin   <- ctime$units %>% str_extract("\\d{2,4}-\\d{1,2}-\\d{1,2}")
    calendar <- ctime$calendar

    nslice <- fid$dim$time$len

    start_time <- 1
    if (ntime == -1) ntime <- nslice

    date <- NULL
    if (check_date) {
        date <- guess_date(nslice, file, calendar, origin, check_date = check_date) # character like that '2010-02-30'
    } else {
        if (!is.null(calendar)) {
            date <- {as.PCICt(origin, cal=calendar) + (fid$dim$time$vals)*86400}
        }
    }

    # time <- ncvar_get(fid, "time")
    if (!missing(DatePeriod) && !is.null(date)){
        DatePeriod = check_DatePeriod(DatePeriod, calendar)
        # get I_time according to period
        I_time <- which(date >= DatePeriod[1] & date <= DatePeriod[2])
        ntime  <- length(I_time) # if empty, NULL will return in L111
        if (ntime > 0) {
            start_time <- I_time[1]
        } else {
            browser() # caution here
        }
    }

    # constrain ntime range
    I_time = NULL
    if (!is.null(varnames[1])) {
        ndim = length(fid$var[[varnames[1]]]$varsize)
        if (ndim >= 3) {
            if (start_time + ntime - 1 > nslice){
                ntime <- nslice - start_time + 1
            }
            I_time <- seq(start_time, start_time+ntime-1)
        }
    }
    date  <- date[I_time] # update time
    start <- c( 1,  1, start_time)
    count <- c(-1, -1, ntime)

    grid.origin <- list(lon = lon, lat = lat, date = date, # , time = time
        dim = c(length(lon), length(lat), nslice),
        origin = origin, calendar = calendar,
        cellsize_x = cellsize_x, cellsize_y = cellsize_y)

    # If range is not empty, then clip it.
    if (!is.null(range)){
        lon_range <- range[1:2] + c(-1, 1)*delta
        lat_range <- range[3:4] + c(-1, 1)*delta

        I_lat <- lat %>% {which(. >= lat_range[1] & . <= lat_range[2]) }
        I_lon <- lon %>% {which(. >= lon_range[1] & . <= lon_range[2]) }

        lat %<>% .[I_lat]
        lon %<>% .[I_lon]

        start <- c(I_lon[1], I_lat[1], start_time)
        count <- c(length(I_lon), length(I_lat), ntime)
    }

    ## fix the error of non-time data
    ndim <- fid$var[[varnames[1]]]$ndims
    if (is.null(ndim)) return(ncvar_get(fid, varnames[1])) # return attributes

    dims = fid$var[[varnames[1]]]$dim %>% sapply(function(x) x$name)
    dim_last = dims[ndim]

    if (dim_last != "time") {
        count[3] = -1
        date = NULL
        ntime = fid$dim[[dim_last]]$len
    }

    vals <- NULL
    if (is.null(ntime) || ntime > 0) {
        # date maybe incorrect if not cmip5 files
        if (!is.null(varnames)){
            # varnames <- names(fid$var) %>% last()
            vals <- lapply(varnames %>% set_names(., .), function(var) {
                ndim <- fid$var[[var]]$ndims
                val <- ncvar_get(fid, var, start[1:ndim], count[1:ndim])*scale+offset
                # if (ntime == 1)
                # constrain value range
                if (!is.null(value_range))
                    val <- clamp(val, value_range, fill.na = TRUE)

                if (!is.null(grid_type)) {
                    if (grid_type == "vec") val <- spdata_array(val, flip = TRUE)
                    dim(val) <- c(length(lat)*length(lon), ntime)
                }
                val
            })
        }
    }

    grid <- list(lon = lon, lat = lat, date = date, # time = time,
        dim = c(length(lon), length(lat), length(date)))
    structure(list(grid.origin = grid.origin, grid = grid, data = vals),
        class = "raster2")
}

check_DatePeriod <- function(DatePeriod, calendar) {
    if (calendar %in% c("360", "360_day")) {
        DatePeriod[2] %<>% gsub("31$", "30", .)
    }
    as.PCICt(DatePeriod, calendar)
    # DatePeriod
}
