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
ncwrite <- function(lst, file,
    var.units = NULL,
    var.longname = NULL,
    prec = "float",
    dims = NULL,
    dimnames_last = NULL,
    range = c(-180, 180, -90, 90),
    dates = NULL,
    calendar = "gregorian",
    missval = -9999L,
    compression = NA,
    scale,
    offset,
    attrs,
    overwrite = FALSE,
    verbose = FALSE)
{
    dim <- dim(lst[[1]])
    if (is.null(dim)) dim <- length(lst[[1]])
    ndim <- length(dim)

    fid <- NULL
    if (file.exists(file) && !overwrite) {
        fid <- nc_open(file, write = TRUE)
        if (is.null(dims)) dims <- fid$dim
    } else {
        if (is.null(dims)) {
            if (ndim > 3 || ndim <= 1) range <- NULL
            # if file not exist
            dims <- ncdim_def_range(dim, range, dates, calendar)
        }
    }

    # 1. check parameters ------------------------------------------------------
    varnames <- names(lst) %>% set_names(., .)
    nvar <- length(varnames)
    if (is.null(var.units)) var.units <- rep("", nvar)
    if (is.null(var.longname)) var.longname <- varnames

    if (length(prec) == 1) prec <- rep(prec, nvar)
    if (length(dimnames_last) == 1) dimnames_last <- rep(dimnames_last, nvar)
    # --------------------------------------------------------------------------

    # 2. define variables
    vars <- lapply(seq_along(varnames), function(i) {
        varname  = varnames[i]
        longname = var.longname[i]
        units    = var.units[i]

        if (!is.null(dimnames_last)) {
            # for spatial data
            dimnames = c("lon", "lat", dimnames_last[i]) %>% rm_empty()
            dim = dims[dimnames]
        } else {
            ndim <- length(dim(lst[[i]])) %>% pmax(1) #
            dim = dims[1:ndim]
        }
        ncvar_def(varname, units, dim, missval, longname,
            prec = prec[i], compression = compression
        )
    })

    # put variables into fid
    if (!file.exists(file) || overwrite) {
        if (file.exists(file) && overwrite) file.remove(file)
        fid <- nc_create(file, vars)
    } else {
        for (var in vars) {
            # if not exists, add new
            if (!(var$name %in% names(fid$var))) {
                fid <- ncvar_add(fid, var)
            }
        }
    }
    on.exit(nc_close(fid))

    # write values
    ncwrite_var(lst, fid, vars, prec, scale, offset)

    # add global attr
    history <- paste("Created by nctools", date(), sep = ", ")
    ncatt_put(fid, 0, "history", history)
    Conventions <- sprintf("nctools version %s", packageVersion("nctools"))
    ncatt_put(fid, 0, "Conventions", Conventions)

    if (!missing(attrs)) {
        for (i in seq_along(attrs)) {
            name = names(attrs)[i]
            ncatt_put(fid, 0, name, attrs[i])
        }
    }
    if (verbose) print(fid)
    invisible()
}

ncwrite_var <- function(lst, fid, vars, prec, scale, offset) {
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
