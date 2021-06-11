#' convert tif into nc files and merge
#'
#' convert tif into nc files and merge by `gdal_translate` and `cdo`
#'
#' @param files tiff file paths
#' @param outfile output nc file
#' @param overwrite If `overwrite`, temporal nc files and `outfile` will be overwrited.
#' @param cache If `cache` = false, temporal nc files will be removed.
#' @param verbose boolean
#'
#' @note This function not works for windows system.
#' @export
tiff2nc_merge <- function(files, outfile = "merged.nc", overwrite = FALSE, cache = TRUE) {
    cat("infiles: \n") # cat(str(files), "\n")
    print(files)

    files_nc <- gsub("\\..{1,4}$", ".nc", files)
    for (infile in files) {
        file_nc <- gsub("\\..{1,4}$", ".nc", infile)
        if (!file.exists(file_nc) || overwrite) {
            cmd <- glue('gdal_translate -ot Float32 -of netCDF -co "FORMAT=NC4" {infile} {file_nc}')
            system(cmd)
        }
    }
    file_nc <- paste(files_nc, collapse = " ")
    if (!file.exists(outfile) || overwrite) {
        system(glue("cdo -f nc4 -z zip_1 cat {file_nc} {outfile}"))
    }
    if (!cache) file.remove(files_nc)
    invisible()
}

#' @examples 
#' \dontrun{
#' tiff2nc_process(files, outfile, var.units, var.longname, run = FALSE)
#' }
#' 
#' @inheritParams ncwrite
#' @import glue
#' 
#' @rdname tiff2nc_merge
#' @export
tiff2nc_process <- function(files, outfile, var.units = NULL, var.longname = NULL, run = FALSE) {
    bands <- raster::brick(files[1]) %>% names()
    cat("bands: ")
    cat(str(bands))

    file_temp <- gsub(".nc", "_temp.nc", outfile)
    # cat(sprintf("varnames: %s", paste(info$varnames, collapse = ", ")))

    years <- str_year(files)
    dates <- seq(as.Date(glue("{min(years)}-01-01")), as.Date(glue("{max(years)}-12-31")), by = "year")
    # dates <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "year")
    cat("dates: \n")
    print(dates)

    if (run) {
        tiff2nc_merge(files, file_temp)
        nc_rewrite(file_temp, outfile, bands, var.units, var.longname, dates)
    }
}
