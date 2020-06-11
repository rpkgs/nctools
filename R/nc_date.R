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

    date <- {as.PCICt(origin, cal=calendar) + (fid$dim$time$vals[1:ntime])*86400} 
    if (to_char) date %<>% format(DATE_FORMAT)
    date
}
