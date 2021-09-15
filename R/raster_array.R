#' @export
raster_array <- function(r) aperm(terra::as.array(r), c(2, 1, 3)) %>% flipud()
