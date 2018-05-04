# wave height- new source??
# 
# http://www.sccoos.org/data/waves/
# USGS Coastal and Marine Geology Program
# http://cmgwindwave.usgsportals.net/
# options chosen in map data: 
# Model Grid: ENP (Eastern North Pacific)
# Model output: US Geophysical Fluid Dynamics Laboratory GFDL-ESM2M
# Location: iooos/USGS station n46219
# Latitude: 33.221 Longitude -119.882

library(ncdf4)

modwaves <- nc_open("data/raw/wave_significant_height_enp_gfdlhistorical.nc")
##[Not run] view structure of NCDF file
# print(modwaves)

times<-ncvar_get(modwaves,"time")

# NCDF is pretty simple (only one longitude/latitude, so the variable wave height is one-dimensional)
heights <- ncvar_get(modwaves,"sea_surface_wave_significant_height")

# Maximum wave height defined as 
# average height of the one third highest waves in the record over the time period
# function takes a vector of wave heights, calculates the 2/3rds quantile, and then
# averages the observations greater than that cutoff
calc_maxHs <- function(Hs) {
  cutoff <- quantile(Hs,2/3)
  mean(Hs[Hs>cutoff])
}
# Convert to data frame
wavesdf <- data_frame(time=times,Hs=heights,date=as_date(as.POSIXct(time,origin="1970-01-01 00:00:00"))) %>%
  #Calculate montly means
  mutate(year=year(date),month=month(date),day=day(date)) %>%
  
  # Calculate the average height of one third highest waves for each month
  group_by(year,month) %>%
  mutate(maxHs=calc_maxHs(Hs)) %>%
  
  # Finally, filter so there's just one value for each month
  distinct(year,month,maxHs) %>%
  ungroup()


# Remove unneeded data from workspace
rm(calc_maxHs,modwaves,times,heights)

### FINISH ###
