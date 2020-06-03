DATE_ORIGIN <- as.POSIXct(0, origin = "1970-01-01", tz = "GMT")

def_time <- function(dates = NULL) {
    times <- difftime(dates, DATE_ORIGIN, units = "days") %>% as.numeric()
    # times <- times[1:dim[ndim]]
    timedim <- ncdim_def("time", "days since 1970-01-01", times, calendar = "gregorian", unlim = TRUE)
    timedim
}

#' @name ncdim_def
#' @title Define a netCDF Dimension
#' 
#' @param ... ignored parameters
#' 
#' @export 
ncdim_def_lonlat <- function(lons, lats, dates = NULL, ...) {
    londim <- ncdim_def("lon", "degrees_east", lons)
    latdim <- ncdim_def("lat", "degrees_north", lats)

    ans = list(lon = londim, lat = latdim, ...)
    if (!is.null(dates)) ans$time = def_time(dates)
    ans
}

# dims <- ncvar_def_sp(dim, range, dates)
#' @rdname ncdim_def
ncdim_def_range <- function(
    dim,
    range = c(-180, 180, -90, 90),
    dates = NULL)
{
    ndim = length(dim) %>% pmax(1)
    # 1. The last dimension is treated as time
    if (is.null(dates)) dates <- as.Date(0:(dim[ndim] - 1), DATE_ORIGIN)
    timedim = def_time(dates)

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

    # Calendar types include 360 day calendars("360_day", "360"), 365 day calendars
    # ("365_day", "365", "noleap"), and Gregorian calendars ("gregorian",
    # "proleptic_gregorian")
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
