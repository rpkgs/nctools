test_that("ncwrite_cmip5 works", {
    set.seed(1)
    nrow <- 70
    ncol <- 40
    ntime <- 3
    range <- c(70, 140, 15, 55)
    arr <- array(rnorm(nrow*ncol*ntime), dim = c(nrow, ncol, ntime))
    
    outfile <- "test-Tmean.nc"
    expect_silent({
        ncwrite_cmip5(list(Tmean = arr, Tmax = arr), outfile)
    })
    
    fid <- nc_open(outfile)
    date <- nc_date(fid, date2str = FALSE)
    expect_equal(date, seq(as.Date('1970-01-01'), as.Date('1970-01-03'), 'day') )
    nc_close(fid)
    
    # test scale and offset
    ncwrite_cmip5(list(Tmean = arr, Tmax = arr), outfile, prec = "integer", scale = 0.01, offset = 10)
    arr2 <- ncread_cmip5(outfile, convertTo2d = FALSE)$data
    max.error <- as.numeric(arr-arr2) %>% abs() %>% max()
    expect_true(max.error <= 0.02)
})
