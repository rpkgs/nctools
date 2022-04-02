guess_varnames <- function(varnames = -1) {
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

#' ncread
#' 
#' @param file file path
#' @param varnames
#' - `-1`: all variables
#' 
#' @export
ncread <- function(file, varnames = 1L) {
  fid <- nc_open(file, readunlim = !check_date)
  on.exit(nc_close(fid)) # make sure closed on error

  varnames %<>% guess_varnames()
  ncvar_get(fid, varnames)
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
