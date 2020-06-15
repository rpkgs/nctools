#' Read data from cmip5 nc files and merge into a large array
#'
#' @param d A data.frame at least with the columns of 'start_adj', 'end_adj' and 'file'`.
#' @inheritParams ncread
#' @param verbose whether to show reading progress?
#' @param bigmemory Boolean. If true, 3d array memory will be pre-allocated.
#' To do this, we need to open nc files first than get the dimension information.
#' This step will sacrifice some time. If false, temporary data are stored in
#' list and finally convert to 3d array.
#'
#' @note Only merge a model for each time. Please make sure that no duplicated date
#' in `d`.
#'
#' @return
#' * `grid.origin`: A list with `'lon', 'lat', 'cellsize_x', 'cellsize_y'`
#'   of original data.
#' * `grid`:  A list with `'lon', 'lat', 'date'` of the read data.
#' * `data`: Merged array value, with the dimension of `[nlon*nlat, ntime]`.
#'
#' @export
#' @importFrom abind abind
#' @importFrom lubridate make_date
#' @import bigmemory
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' range <- c(15, 55, 70,140)
#'
#' d_files   <- CMIP5Files_filter(files)
#' lst_dfiles <- d_files %>% split(., .$model)
#'
#' i = 1
#' d <- lst_dfiles[[i]]
#'
#' l <- nc_merge(d, varname = 'tasmax', range, delta = 2)
#' }
nc_merge <- function(d,
    varname = 1L,
    range,
    delta = 2,
    verbose = TRUE,
    bigmemory = FALSE,
    scale=1,
    offset=0,
    value_range=NULL,
    adjust_lon = TRUE) # , ntime = -1
{
    options(bigmemory.typecast.warning=FALSE)

    model <- d$model[1]
    nfile <- nrow(d)
    if (verbose)
        ok(sprintf('=========== %s [n=%d] ===========\n', model, nfile))

    # convertTo2d <- ifelse(bigmemory, TRUE, FALSE)

    if (bigmemory){
        # 1. get grid size, [nlon, nlat]
        i = 1
        DatePeriod <- c(d$start_adj[i], d$end_adj[i]) %>% format(DATE_FORMAT)
        file       <- d$file[i]

        l <- ncread(file, varname = NULL, range = range, delta = delta,
            ntime = -1, DatePeriod = DatePeriod,
            grid_type = NULL,
            scale = scale, offset = offset, value_range = value_range,
            adjust_lon = adjust_lon, check_date = TRUE)

        dim <- l$grid$dim
        nij <- prod(dim[1:2])

        # 2. get ntime_max and allocate memory according to it
        ntime_max <- d %$% {
            # note: ymd will be error, when year < 100
            # start_date <- start_adj %>% pmax(min(start))
            # end_date   <- end_adj   %>% pmin(max(end))
            # c(start_date, end_date)
            difftime(max(end_adj), min(start_adj), "day") %>% {as.numeric(.)+1}
        }

        cat(ok(sprintf("\t%s | dim=[%d, %d, %d], about %.1fG\n",
                model, dim[1], dim[2], ntime_max, nij*ntime_max*8/1024^3)))

        if (nfile == 1) ntime_max <- l$grid$dim[3]
        array_val <- big.matrix(nij, ntime_max, "float") # default type `double`
        k_start   <- 0
    }

    lst_date <- lst <- list()
    for (i in 1:nfile){
        DatePeriod <- c(d$start_adj[i], d$end_adj[i]) %>% format(DATE_FORMAT)
        file       <- d$file[i]

        if (verbose) {
            cat(sprintf('[%02d] reading %s %s ... \n', i, basename(file),
                green(file_size(file))))
        }

        l <- ncread(file, varname = varname, range = range, delta = delta,
            ntime = -1, DatePeriod = DatePeriod,
            grid_type = "mat",
            scale = scale, offset = offset, value_range = value_range,
            adjust_lon = adjust_lon, check_date = TRUE)
        lst_date[[i]] <- l$grid$date

        if (bigmemory) {
            # note that value are 2d matrix in bigmemory
            # fill values int `array_val`
            ntime_i <- length(l$grid$date)
            I_time  <- seq.int(k_start+1, k_start + ntime_i)
            ## TODO: solve the problem of missing year
            array_val[, I_time] <- l$data[[1]]
            k_start <- k_start + ntime_i
        } else {
            lst[[i]] <- l
        }
    }

    if (!bigmemory){
        along <- dim(lst[[1]]$data[[1]]) %>% length()
        array_val <- map(lst, ~.$data[[1]]) %>% abind(along = along)
    } else {
        array_val <- sub.big.matrix(array_val, firstCol = 1, lastCol = k_start)
    }

    date_all <- do.call(c, lst_date)
    I_dup    <- duplicated(date_all) %>% which()

    if (!is_empty(I_dup)) {
        # date_all %<>% .[-I_dup]
        stop(sprintf('[nc] %s, duplicated date found!', model))
        # rm duplicated date
        # array_val <- array_val[,,I_nodup]
    }

    # get dimension infomation
    grid.origin <- l$grid.origin[c('lon', 'lat', 'dim','cellsize_x', 'cellsize_y')]
    grid        <- l$grid[c('lon', 'lat', 'date', 'dim')]

    grid.origin$dim[3] <- length(date_all)
    grid$dim[3]        <- length(date_all)

    grid$date <- date_all
    grid$object_size <- utils:::format.object_size(prod(grid$dim) * 4, "auto") # single in cpp?
    # rm(lst, l); gc()
    structure(list(grid.origin = grid.origin, grid = grid, data = array_val),
        class = "raster2")
}
