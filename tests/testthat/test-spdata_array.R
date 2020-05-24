test_that("spdata_array works", {
    range = c(73, 105, 25, 40)
    set.seed(1)
    nrow = 48000
    mat = matrix(rnorm(nrow*2), nrow, 2)
    r1  <- spdata_array(mat, range, cellsize = 0.1)
    r12 <- spdata_array(mat[,1], range, cellsize = 0.1)
    # image(r1[,,1])
    expect_equal(r1[,,1, drop = FALSE], r12)

    r2 <- spdata_array(r1[,,1])
    r3 <- spdata_array(r1[,,1, drop = FALSE])

    expect_equal(r2, r3)
    # TRUE
})
