nc_aggregate_mat <- function(file, vars_mean = NULL, is_save = TRUE, overwrite = TRUE) {
    outfile <- gsub(".nc$", "_yearly.nc", file)
    if (file.exists(file) && !overwrite) return()

    varnames <- nc_info(file, verbose = FALSE)
    # mat <- apply_row(l$GPP, by = year(dates))
    dates <- nc_date(file)
    dates_new <- make_date(sort(unique(year(dates))))

    l <- ncread_cmip(file, -1)$data

    res <- rep(list(NULL), length(varnames)) %>% set_names(varnames)
    for(i in seq_along(varnames)) {
        varname <- varnames[i]
        type = if (varname %in% vars_mean) "mean" else "sum"
        res[[i]] <- aggregate_yearly(l[[i]], dates, type)
    }

    if (is_save && (!file.exists(file) || overwrite) ) {
        ncwrite(res, outfile,
            range = NULL, dates = dates_new,
            overwrite = overwrite)
    } else res
}

#' aggregate 8-day 3d array to yearly
#' @export
aggregate_yearly <- function(array, dates, type = c("sum", "mean")[1]) {
    years <- year(dates)
    # year_begin = first(years)
    year_end  <- years[length(years)]
    delta_days <- c(dates, make_date(year_end + 1, 1, 1)) %>% diff() %>% as.numeric()

    if (type == "sum") {
        vals = apply_3d(array, FUN = weightedSum, by = years, w = delta_days)
    } else if (type == "mean") {
        vals = apply_3d(array, FUN = weightedMean, by = years, w = delta_days)
    }
    vals
}

#' This function can't remove NA values
weightedSum <- function(x, w = NULL, idxs = NULL, na.rm = FALSE, ...) {
    ans <- x %*% as.matrix(w)
    # browser()
    ans
}

weightedMean <- function(x, w = NULL, idxs = NULL, na.rm = FALSE, ...) {
    w = w/sum(w)
    ans <- x %*% as.matrix(w)
    # browser()
    ans
}

is_leapyear <- function(year) {
    (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

summary_list <- function(l) {
    if (is.array(l)) return(summary(as.numeric(l)))

    lapply(l, function(.x) summary(as.numeric(.x)))
}
