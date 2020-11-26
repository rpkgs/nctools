DATE_FORMAT <- "%Y-%m-%d"

#' guess CMIP5 date according to nc file name
#'
#' @inheritParams ncread
#' @param nslice length of time in original nc file.
#' @param calendar the calendar type.
#'
#' @return PCICt date vector
#'
#' @keywords internal
#' @export
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' guess_date(nslice, file, calendar = "365")
#' }
guess_date <- function(nslice, file, calendar = "365", origin, check_date = FALSE){
    date_file <- basename(file) %>% {str_extract_all(., "\\d{8}")[[1]]} %>%
        str_replace_all(., "(?<=^\\d{4})|(?<=^\\d{6})", "-")
    origin2    <- date_file[1]
    if (is.na(origin2) && !is.na(origin)) {
        origin2 <- origin
    }

    # calendar maybe missing
    date <- tryCatch({
        as.PCICt(origin2, cal=calendar) + (0:(nslice-1))*86400
    }, error = function(e){
        message(sprintf("[nc_date] %s, %s\n", basename(file), e$message))
    })

    date_band <- date[c(1, length(date))] %>% format(DATE_FORMAT)

    if (is.null(date)) {
        warning(sprintf("[nc_date] %s, %s", basename(file), "file to generate date!"))
        return(NULL)
    }

    if (check_date) {
        # check date | 1. duplicated date，(e.g. BNU-ESM)
        if (!all(date_file == date_band)){
            simpleError(sprintf("[nc_date] %s, %s", basename(file), "calendar type error in nc file!"))
        }
    }
    return(date)
}
