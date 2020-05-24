test_that("spdata_array works", {
    range = c(73, 105, 25, 40)
    cellsize = 0.1
    lon = seq(range[1] + cellsize/2, range[2], cellsize)
    lat = seq(range[3] + cellsize/2, range[4], cellsize)

    set.seed(1)
    nrow = 48000
    mat = matrix(rnorm(nrow*2), nrow, 2)
    r1 <- spdata_array(mat, range, cellsize = 0.1)
    r2 <- spdata_array(mat, nlon = length(lon), nlat = length(lat))

    r12 <- spdata_array(mat[,1], range, cellsize = 0.1)
    r13 <- spdata_array(mat[,1], nlon = length(lon), nlat = length(lat))

    # image(r1[,,1])
    expect_equal(r1, r2)
    expect_equal(r1[,,1, drop = FALSE], r12)
    expect_equal(r12, r13)

    ## test about 3d array
    r2_1 <- spdata_array(r1[,,1])
    r2_2 <- spdata_array(r1[,,1, drop = FALSE])
    expect_equal(r2_1, r2_2)
    # r2_3 = spdata_array(r1[,,1], nlon = length(lon), nlat = length(lat))
    # expect_equal(r2_1, r2_3)
    # TRUE
})
