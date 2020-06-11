# covr::package_coverage()
test_that("ncread and ncwrite works", {
    expect_true({
        ## 1. write netcdf file
        file <- 'test_real3d.nc'

        range = c(-180, 180, -90, 90)

        varname = c("Temperature")
        var.units = c("K")
        missval = c(1.e30)
        dates = as.Date("2000-01-01")

        # Put some test data in the file
        nx = 360; ny = 180
        data_temp <- array(0.,dim=c(nx,ny,1))
        for( j in 1:ny )
            for( i in 1:nx )
                data_temp[i,j,1] <- sin(i/10)*sin(j/10)

        ncwrite(list(Temperature = data_temp), file, var.units, missval = missval,
                dates = dates,
                verbose = FALSE, overwrite = TRUE)

        #===========================================================================
        # PART 2.  ADD A NEW VARIABLE TO THE FILE
        ncwrite(list(Temperature2 = data_temp), file, var.units, missval = missval,
                dates = dates ,
                verbose = FALSE, overwrite = FALSE)
        expect_equal(format(dates), format(nc_date(file)))
        ## 2. read netcdf file
        l <- ncread(file, c(1, 2))
        file.remove(file)
        TRUE
    })
    # expect_equal(2 * 2, 4)

    ## test non-raster data ----------------------------------------------------
    expect_true({
        file = "test-non_raster2.nc"
        mat <- rnorm(400) %>% matrix(100, 4)
        # error
        ncwrite(list(x1 = mat[, 1:4]), file,
                range = NULL,
                prec = "short",
                scale = 0.001, offset = 0,
                # dates = as.Date("2000-01-01"),
                verbose = FALSE, overwrite = TRUE)
        ncwrite(list(x3 = mat[,1]), file,
                range = NULL,
                prec = "short",
                scale = 0.001, offset = 0,
                # dates = as.Date("2000-01-01"),
                verbose = FALSE, overwrite = FALSE)
        l <- ncread(file, -1)

        ## second solution
        dates <- seq(as.Date("2000-01-01"), as.Date('2000-01-04'), by = "day")
        ncwrite(list(x1 = mat[, 1:4], x3 = mat[,1]), file,
                range = NULL,
                dates = dates,
                verbose = FALSE, overwrite = TRUE)
        # print(dates)
        # print(nc_date(file))

        # expect_equal(dates, nc_date(file))
        file.remove(file)
        TRUE
    })
})
