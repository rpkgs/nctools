## 1. define time dimension
dates = seq(as.Date("2010-01-01"), as.Date("2010-12-01"), by = "month")
dates_str = format(dates)

# both of `date` or `string` all work
t1 = ncdim_def_time(dates)
t2 = ncdim_def_time(dates_str)
str(t1)
all.equal(t1, t2)

## 2. define by range
dim  = c(360, 180, 12)
dims_2 = ncdim_def_range(dim) %>% str()

## 3. define by lat and lon
dims_3 = ncdim_def_lonlat(-180:180, -90:90) %>% str()


library(rgdal)
file = "H:/global_WB/ET/diagnosed_PML/v017/PMLV2_yearly_v17_ERA5L_2005-01-01.nc"
