# Wave height data from Tom Bell

require(tidyverse)
require(lubridate)

hs <- read_csv("data/raw/SanNic_Hs_v2.csv",col_types = "iiiidd") %>%
  # Site 2 is the West End of SNI
  select(Year,Month,Day,Mean_2,Max_2) %>%
  unite("date",Year,Month,Day,sep="-",remove=F)%>%
  mutate(date=as.Date(date)) %>%
  rename(maxHs=Max_2)%>%

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

# Convert to data frame
wavesdf <- data_frame(time=times,Hs=heights,date=as_date(as.POSIXct(time,origin="1970-01-01 00:00:00"))) %>%
  mutate(year=year(date),month=month(date),day=day(date)) %>%
  
  # Calculate the max Hs for each day
  group_by(year,month,day) %>%
  summarise(maxHs_usgs=max(Hs)) %>%
  ungroup()

# Remove unneeded data from workspace
rm(modwaves,times,heights)


# join and compare 2 datasets #
wavesdf <- hs %>%
  full_join(wavesdf,by=c("year","month","day")) %>%

  ungroup()
# 
# wavesdf %>%
#   mutate(day=1) %>%
#   unite(date,year,month,day,sep="-")%>%
#   mutate(date=as.Date(date))%>%
#   gather(key="dataset",value="height",maxHs,maxHs_usgs)%>%
#   ggplot(aes(date,height,color=dataset))+
#   geom_line()+
#   scale_color_discrete(labels=c("Bell","USGS"))+
#   theme_minimal()

# # plot over time
# wavesdf %>%
#   unite(date,year,month,day,sep="-")%>%
#   mutate(date=as.Date(date))%>%
#   gather(key="dataset",value="height",maxHs,maxHs_usgs)%>%
#   ggplot(aes(date,height,color=dataset))+
#   geom_line()+
#   scale_color_discrete(labels=c("Bell","USGS"))+
#   theme_minimal()
# 
# wavesdf %>%
#   ggplot(aes(maxHs,maxHs_usgs))+
#   geom_point()+
#   geom_abline(slope=1,intercept=0,linetype=2)+
#   labs(title="Max Daily Wave Height",x="Bell height",y="USGS height")+
#   theme_minimal()

# coalesce data (fill in Bell data with USGS data) and find monthly means
# Monthly mean of daily Max Hs
wavesdf <- wavesdf %>%
  group_by(year,month) %>%
  mutate(maxHs_comb=coalesce(maxHs,maxHs_usgs)) %>%
  group_by(year,month) %>%
  summarise(maxHs=mean(maxHs_comb,na.rm=T)) %>%
  ungroup()

rm(hs)
### FINISH ###