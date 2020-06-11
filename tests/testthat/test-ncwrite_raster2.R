# library(lattice)
# library(sp2)

test_that("multiplication works", {
    infile = system.file("extdata/HI_Threshold_MIROC-ESM.RDS", package = "nctools")
    r <- readRDS(infile)
    outfile = "test-ncwrite_raster.nc"
    ncwrite_raster2(r, outfile)
    names = nc_info(outfile, FALSE)
    expect_equal(setNames(names, NULL), c("TRS", "mean.T_degC", "mean.T_HI"))
    file.remove(outfile)
})

# usethis::use_data(DATASET, overwrite = TRUE)
# levelplot(mean.T_degC[,,1])
# set_dim_raster2 <- function(x, nlon, nlat) {
#     dim <- dim(x)
#     ndim = pmax(dim[length(dim)], 1)
#     set_dim(x, c(nlon, nlat, ndim))
# }
