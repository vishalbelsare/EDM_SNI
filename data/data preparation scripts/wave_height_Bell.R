# Wave height data from Tom Bell

require(tidyverse)
require(lubridate)

hs <- read_csv("data/raw/SanNic_Hs.csv") %>%
  # Site 2 is the West End of SNI
  select(Year,Month,Day,Max_2) %>%
  unite("date",Year,Month,Day,sep="-",remove=F)%>%
  mutate(date=as.Date(date)) %>%
  rename(maxHs=Max_2)%>%
  # Monthly mean of daily Max Hs
  # group_by(Year,Month) %>%
  # summarise(maxHs=mean(Max_2,na.rm=T)) %>%
  rename(year=Year,month=Month,day=Day)%>%
  ungroup()
hs$maxHs[is.na(hs$maxHs)]<-NA

### USGS DATA TO COMPARE ###

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
  group_by(year,month,day) %>%
  summarise(maxHs_usgs=max(Hs)) %>%
  ungroup()
  # group_by(year,month) %>%
  # summarise(maxHs_usgs=mean(maxHs,na.rm=T)) %>%
  # ungroup()


# Remove unneeded data from workspace
rm(calc_maxHs,modwaves,times,heights)

### FINISH ###

# compare #
wavesdf <- hs %>%
  full_join(wavesdf,by=c("year","month","day")) %>%
  # mutate(maxHs=coalesce(maxHs,maxHs_usgs)) %>%
  # select(year,month,maxHs)%>%
  # arrange(year,month) %>%
  ungroup()

wavesdf %>%
  mutate(day=1) %>%
  unite(date,year,month,day,sep="-")%>%
  mutate(date=as.Date(date))%>%
  gather(key="dataset",value="height",maxHs,maxHs_usgs)%>%
  ggplot(aes(date,height,color=dataset))+
  geom_line()+
  scale_color_discrete(labels=c("Bell","USGS"))+
  theme_minimal()

# Comparison check
wavesdf %>%
  unite(date,year,month,day,sep="-")%>%
  mutate(date=as.Date(date))%>%
  gather(key="dataset",value="height",maxHs,maxHs_usgs)%>%
  ggplot(aes(date,height,color=dataset))+
  geom_line()+
  scale_color_discrete(labels=c("Bell","USGS"))+
  theme_minimal()
