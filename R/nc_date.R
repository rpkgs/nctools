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
nc_date <- function(fid, ntime = -1, unit = NULL, to_char = FALSE){
    if (class(fid)[1] != "ncdf4") {
        fid <- nc_open(fid)
        on.exit(nc_close(fid))
    }

    ctime    <- fid$dim$time # time class
    # origin   <- ctime$units %>% str_extract("\\d{2,4}-\\d{1,2}-\\d{1,2}")
    origin <- ctime$units %>% str_extract("(?<=since ).*")
    calendar <- ctime$calendar
    
    if (is.null(calendar)) calendar <- "gregorian"
    nslice <- fid$dim$time$len
    if (ntime == -1) ntime <- nslice

    if (is.null(unit)) unit = fid$dim$time$units %>% str_extract(".*(?= since)")
    secs <- unit_second(unit)
    date <- {as.PCICt(origin, cal=calendar) + (fid$dim$time$vals[1:ntime])*secs}
    if (to_char) date %<>% format(DATE_FORMAT)
    date
}

unit_second <- function(unit) {
    switch(unit,
        "seconds" = 1,
        "hours" = 3600,
        "days"  = 86400
    )
}
