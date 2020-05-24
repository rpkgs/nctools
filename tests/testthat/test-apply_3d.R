test_that("apply_3d works", {
    library(matrixStats)
    
    # apply_3d also suit for 2d matrix
    mat <- matrix(rnorm(100), 4, 300)
    r1 <- apply_3d(mat, 2, by = rep(c(1, 2, 3), each = 100))
    expect_equal(dim(r1), c(4, 3))


    array <- array(rnorm(100), dim = c(4, 300, 5))
    r2 <- apply_3d(array, 3)
    expect_equal(dim(r2), dim(array)[-3])

    r3 <- apply_3d(array, 2)
    expect_equal(dim(r3), dim(array)[-2])
    # expect_equal(2 * 2, 4)
})
