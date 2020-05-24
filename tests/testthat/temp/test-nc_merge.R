context("nc_merge")


test_that("multiplication works", {
    skip_if_not(isTestNC)
    # skip_if_not(FALSE)
    # global variables, e.g. files_r1i1p1 in `helper-nc.R`
    # files_i <- files_r1i1p1$piControl
    files_i = dir("J:/CMIP5/historicalGHG_tasmax", full.names = TRUE)
    
    d_FileSelected   <- CMIP5Files_filter(files_i)
    lst_FileSelected <- d_FileSelected %>% split(., .$model) 
    
    d <- lst_FileSelected$`ACCESS1-3`[1:4, ]

    print(d)
    # main test -------------------------------------------------------------
    p1 <- peakRAM::peakRAM(l1 <- nc_merge(d, range, delta = 2, 
                                          varname = 'tasmax', bigmemory = T, verbose = TRUE))
    l2 <- nc_merge(d, range, delta = 2, varname = 'tasmax', bigmemory = F, verbose = TRUE)
    
    dim <- l1$grid$dim
    expect_true(p1$Peak_RAM_Used_MiB < prod(dim)*4/1024^2*1.3)
    
    expect_equal(dim(l1$value), c(prod(dim[1:2]), dim[3]))
    expect_equal(dim(l2$value), dim)
})
