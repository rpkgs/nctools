guess_varnames <- function(fid, varnames = -1) {
  # fid <- nc_open(file)
  # on.exit(nc_close(fid)) # make sure closed on error

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
  varnames
}

check_period <- function(period, calendar = "gregorian") {
  if (is.null(period)) return(NULL)
  if (length(period) == 1) period = rep(period, 2)
  if (is.numeric(period)) {
    period = c(make_date(period[1]), make_date(period[2], 12, 31)) |> as.character()
  }
  
  if (calendar %in% c("360", "360_day")) {
      period[2] %<>% gsub("31$", "30", .)
  }
  as.PCICt(period, calendar)
}

check_DatePeriod = check_period

#' ncread
#' 
#' @param file file path
#' @param varnames
#' - `-1`: all variables
#' 
#' @param ... others to [ncvar_get()]
#' 
#' @export
ncread <- function(file, varnames = 1L, period = NULL, ...) {
  fid <- nc_open(file)
  on.exit(nc_close(fid)) # make sure closed on error

  varnames %<>% guess_varnames(fid, .)

  date = nc_date(file)  
  if (!is.null(period) && !is.null(date)) {
    period <- check_period(period)
    
    # get I_time according to period
    I_time <- which(date >= period[1] & date <= period[2])
    ntime <- length(I_time) # if empty, NULL will return in L111
    start_time <- I_time[1]

    start <- c(1, 1, start_time)
    count <- c(-1, -1, ntime)

    ncvars_get(fid, varnames, start = start, count = count, ...)  
  } else {
    ncvars_get(fid, varnames, ...)
  }
}

ncvars_get <- function(fid, varnames, ...) {
  ans = lapply(varnames %>% set_names(., .), function(var) {
        ncvar_get(fid, var, ...)
  })
  if (length(varnames) == 1) ans = ans[[1]]
  ans
}
  

#' @rdname ncread
#' @export
ncread_all <- function(file) {
  varnames = nc_info(file, FALSE) %>% setdiff("crs")
  fid = nc_open(file)
  on.exit(nc_close(fid))

  cat(sprintf("Reading all variables: %s ... \n", basename(file)))
  plyr::llply(varnames, function(var) ncvar_get(fid, var), .progress = "text")
}
