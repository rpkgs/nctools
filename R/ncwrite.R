#' Write CMIP5 nc file
#'
#' @inheritParams ncread
#' @inheritParams ncdf4::ncvar_def
#'
#' @param lst A named list of 3d arrays, which have the dimension of `[nlon, nlat, ntime]`.
#' `image(lst[[1]][,,1])` should looks normal.
#'
#' @param file Name of the netCDF file to be created.
#' @param var.units untis of variables in `lst`
#' @param var.longname longname of variables in `lst`
#' @param prec Precision of the created variable. Valid options: 'short' 'integer'
#' 'float' 'double' 'char' 'byte'. See the special note below for how to create
#' a character variable (strings).
#' short: -32768 - 32768.
#'
#' @param dates A `Date` or `POSIXct` object, specified corresponding date
#' @param attrs A named list of global attributes, e.g. title, author, souce,
#' references, institution.
#' @param scale,offset If the variable in the netCDF file has a scale and/or offset
#' attribute defined, the returned data are automatically and silently scaled
#' and/or offset as requested. Then you should just leave scale = 1,
#' and offset = 0.
#' @param range A numeric vector, `[lon_min, lon_max, lat_min, lat_max]`.
#'
#' @seealso [ncread], [ncdf4::ncvar_def]
#'
#' @examples
#' \dontrun{
#' range_china = c(70, 140, 15, 55)
#' ncwrite(list(Tmin = arr[,,1:2]), "Tmin.nc", "degC", range = range_china,
#'      prec = "short", scale = 0.01, offset = 10,
#'      dates = dates)
#' }
#'
#' @import ncdf4
#' @importFrom utils packageVersion
#' @export
ncwrite <- function(lst = NULL,
    file,
    var.units = NULL,
    var.longname = NULL,
    prec = "float",
    range = c(-180, 180, -90, 90),
    dates = NULL,
    missval = -9999L,
    compression = NA,
    scale,
    offset,
    attrs,
    overwrite = FALSE,
    verbose = FALSE)
{
    # 0. check parameters ------------------------------------------------------
    varname <- names(lst) %>% set_names(., .)
    nvar <- length(varname)
    if (is.null(var.units))    var.units <- rep("", nvar)
    if (is.null(var.longname)) var.longname <- varname

    if (length(prec) == 1) prec <- rep(prec, nvar)

    dim <- dim(lst[[1]])
    if (is.null(dim)) dim <- length(lst[[1]])

    ndim <- length(dim)
    if (ndim > 3 || ndim <= 1) range = NULL

    fid <- NULL
    if (file.exists(file) && !overwrite) {
        fid <- nc_open(file, write = TRUE)
        dims <- fid$dim
    } else {
        dims <- ncvar_def_sp(dim, range, dates)
    }

    # 2. define variables
    vars <- lapply(seq_along(varname), function(i) {
        name  = varname[i];  longname = var.longname[i]
        units = var.units[i]
        ndim <- length(dim(lst[[i]])) %>% pmax(1) #
        ncvar_def(name, units, dims[1:ndim], missval, longname,
            prec=prec[i], compression = compression)
    })

    if (!file.exists(file) || overwrite) {
        if (file.exists(file) && overwrite) file.remove(file)
        fid <- nc_create(file, vars)
    } else {
        for(var in vars) {
            # if not exists, add new
            if (!(var$name %in% names(fid$var))) {
                fid <- ncvar_add( fid, var )
            }
        }
    }
    on.exit(nc_close(fid))
    # fill in values
    for (i in seq_along(vars)) {
        varid = vars[[i]]
        # 1. add scale and offset
        scale_i  <- 1
        offset_i <- 0
        if (!missing(scale)) {
            scale_i <- if (length(scale) == 1) scale else scale[i]
            ncatt_put(fid, varid$name, "scale_factor", scale_i, prec="double")
        }

        if (!missing(offset)) {
            offset_i <- if (length(offset) == 1) offset else offset[i]
            ncatt_put(fid, varid$name, "add_offset", offset_i, prec="double")
        }

        # 2. put values
        vals <- (lst[[i]] - offset_i) / scale_i
        # convert type
        if (prec[i] %in% c("integer", "short")) { 
            # mode(vals) <- "integer"
            vals <- round(vals)
            # 避免精度在小数点保留位数上出现损失
        }        
        ncvar_put(fid, varid, vals)
    }

    # add global attr
    history <- paste("Created by rPML", date(), sep=", ")
    ncatt_put(fid, 0, "history",history)
    Conventions <- sprintf("rPML version %s", packageVersion("rPML"))
    ncatt_put(fid, 0, "Conventions", Conventions)

    if (!missing(attrs)) {
        for (i in seq_along(attrs)) {
            name = names(attrs)[i]
            ncatt_put(fid, 0, name, attrs[i])
        }
    }
    # put additional attributes into dimension and data variables
    # ncatt_put(fid,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
    # ncatt_put(fid,"lat","axis","Y")
    # ncatt_put(fid,"time","axis","T")
    # # add global attributes
    # ncatt_put(fid, 0,"institution", "Sun Yat-sen University")
    # ncatt_put(fid,0,"source",datasource$value)
    # ncatt_put(fid,0,"references",references$value)
    # ncatt_put(fid,0,"Conventions",Conventions$value)
    # ncatt_put(fid, 0, "range", range)
    if (verbose) print(fid)
    invisible()
}


# ' @param range 2 numeric vector
get_coord <- function(range, length) {
    from <- range[1]
    to   <- range[2]
    values   <- seq(from, to, length.out = length + 1)
    cellsize <- diff(values[1:2])
    values[-1] - cellsize/2
}

# ndim = 2 or 3
ncvar_def_sp <- function(
    dim,
    range = c(-180, 180, -90, 90),
    dates = NULL)
{
    ndim = length(dim) %>% pmax(1)
    # 1. The last dimension is treated as time
    date.origin <- as.POSIXct(0, origin = "1970-01-01", tz = "GMT")
    if (is.null(dates)) {
        dates <- as.Date(0:(dim[ndim] - 1), date.origin)
    }
    times <- difftime(dates, date.origin, units = "days") %>% as.numeric()
    times <- times[1:dim[ndim]]
    timedim <- ncdim_def("time", "days since 1970-01-01", times, calendar = "gregorian", unlim = TRUE)

    # 1. define dimensions -----------------------------------------------------
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

#' @param r `raster2` object returned by [ncwrite()]
#' @param varname variable name in the data of `r`
#' @param ... other parameters to [ncwrite]
#' 
#' @rdname ncwrite
#' @export
ncwrite.raster2 <- function(r, file, varname = "array", ...){
    # only surport regular grids
    lons <- r$grid$lon
    lats <- r$grid$lat

    cellsize_x <- diff(lons[1:2])
    cellsize_y <- diff(lats[1:2])

    lon_range <- lons[c(1, length(lons))] + c(-1, 1)*cellsize_x/2
    lat_range <- lats[c(1, length(lats))] + c(-1, 1)*cellsize_y/2

    range <- c(lon_range, lat_range)

    lst_nc <- list(r$data) %>% set_names(varname)

    ncwrite(lst_nc, file, dates = r$grid$date, range = range, ...)
}
