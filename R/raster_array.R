#' @export
#' @importFrom terra as.array
raster_array <- function(r) aperm(as.array(r), c(2, 1, 3)) %>% flipud()
