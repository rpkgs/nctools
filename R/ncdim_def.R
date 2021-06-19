#' @name ncdim_def
#' @title Define a netCDF Dimension for `ncwrite`
#' 
#' @param ... ignored parameters
#' 
#' @export 
ncdim_def_lonlat <- function(lons, lats, dates = NULL, ...) {
    londim <- ncdim_def("lon", "degrees_east", lons)
    latdim <- ncdim_def("lat", "degrees_north", lats)

    ans = list(lon = londim, lat = latdim, ...)
    if (!is.null(dates)) ans$time = ncdim_def_time(dates)
    ans
}

# ' @description 
# ' - `ncdim_def_time`: define time dimension
#' @example R/examples/ex-ncdim.R
#' 
#' @rdname ncdim_def
#' @export
ncdim_def_time <- function(dates = NULL) {
    if (!is(dates, "PCICt")) dates = as.PCICt(format(dates), cal = "gregorian")
    
    calendar = attr(dates, "cal")
    times <- as.numeric(dates) / 86400 # convert to days
    timedim <- ncdim_def("time", "days since 1970-01-01", times, calendar = calendar, unlim = TRUE)
    timedim
    # if (is.null(calendar)) calendar <- "gregorian"
    # DATE_ORIGIN = as.PCICt("1970-01-01", cal = calendar) # "360_day"
    # times <- difftime(dates, DATE_ORIGIN, units = "days") %>% as.numeric()
}

# dims <- ncvar_def_sp(dim, range, dates)

#' @param dim dimension of nc.array, `[nlon, nlat, ntime]`
#' @param dates PCICt, Date or Date string object. The default origin is '1970-01-01'
#' @param range `[lon_min, lon_max, lat_min, lat_max]`
#' @param calendar Calendar types include 360 day calendars("360_day",  "360"), 
#' 365 day calendars ("365_day", "365", "noleap"), and Gregorian calendars 
#' ("gregorian", "proleptic_gregorian").
#' 
#' @rdname ncdim_def
#' @export 
ncdim_def_range <- function(
    dim,
    range = c(-180, 180, -90, 90),
    dates = NULL, 
    calendar = "gregorian", 
    date_origin = "1970-01-01", by = "month")
{
    if (is.null(calendar)) calendar <- "gregorian"
    DATE_ORIGIN = as.PCICt("1970-01-01", cal = calendar) # "360_day"

    ndim = length(dim) %>% pmax(1)
    # 1. The last dimension is treated as time
    if (is.null(dates)) {
        dates = seq(as.Date(date_origin), by = by, length.out = dim[ndim])
    }
    timedim = ncdim_def_time(dates)

    # 2. define dimensions -----------------------------------------------------
    if (!is.null(range)) {
        lons <- get_coord(range[1:2], dim[1])
        lats <- get_coord(range[3:4], dim[2])
        # define dimensions
        londim <- ncdim_def("lon", "degrees_east", lons)
        latdim <- ncdim_def("lat", "degrees_north", lats)
        dims <- list(lon = londim, lat = latdim, time = timedim)
    } else {
        dimnames <- c("x", "y", "z")
        if (ndim <= 3) {
            dims <- lapply(1:ndim, function(i) { ncdim_def(dimnames[i], "-", 1:dim[i]) })
            dims[[ndim]] <- timedim
            # names(dims)[ndim] <- "time"
        } else {
            stop("ndim should be less than 3!")
        }
    }
    dims
}

# ' @param range 2 numeric vector
get_coord <- function(range, length) {
    from <- range[1]
    to   <- range[2]
    values   <- seq(from, to, length.out = length + 1)
    cellsize <- diff(values[1:2])
    values[-1] - cellsize/2
}
