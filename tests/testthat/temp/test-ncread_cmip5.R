context("ncread_cmip5")

test_that("multiplication works", {
    # skip_if_not(isTestNC)
    ncfile <- system.file("extdata/nc/test-Tmean.nc", package = "CMIP6tools")
    cat(sprintf("\ntest ncfile: %s ...\n", basename(ncfile)))
    
    # no varname no value returned
    l1 <- ncread_cmip5(ncfile)
    # expect_null(l1$value)
    
    # varname default: 1
    expect_silent(l2 <- ncread_cmip5(ncfile, ntime = 1, varname = "Tmean"))
    expect_silent(l3 <- ncread_cmip5(ncfile, ntime = 2, varname = 1, convertTo2d = FALSE))
    expect_silent(l4 <- ncread_cmip5(ncfile, ntime = 1, varname = -1))
    
    expect_type(l2$grid$date, "character")
    # print(str(l3, 2))
    # expect_null(l4$grid$date)
})
