# Physical/Oceanographic Data Preparation for San Nicolas Island EDM analysis

#### Introduction ####
# The following code takes raw physical datasets and matches them to the same timescale as the SNI community benthic monitoring datasets.  
# There are four physical variables to be matched:
#   
# * The Multivariate ENSO index
# * The Pacific Decadal Oscillation
# * The North Pacific Gyre Oscillation
# * Sea Surface Temperature and Significant Wave Height
# 
# For each, we:
# 1. Match them to the monitoring "periods" from the SNI benthic monitoring datasets. For the purpose of using physical variables
#    to predict biological variables, we offset the periods such that physical variables applied to each period are actually
#    the mean value for the four months preceding and during the monitoring period. For example, the first monitoring period
#    in the SNI dataset is Fall, 1980. The physical variables that would apply to this period are the data from summer, i.e. Jun-Sep 1980.
#    Similarly, for the Spring monitoring periods, the physical variables are averaged from the previous Dec-March.
# 2. Normalize the time series to zero mean and unit variance, just as we did with biological variables

# Finally, we join the datasets together, aligning all relevant physical data by monitoring period

# Required libraries
library(tidyverse)
library(lubridate)

#### Period matching key ####
# Have to line up the periods such that December is included in the next Spring's period
period.key <- data_frame(year=rep(1980:2014,each=12),month=rep(1:12,35),period=c(rep(NA,5),rep(1,6),rep(2:69,each=6),NA))

# Set months that are not in Dec-Mar or Jun-Sep as NA so their data aren't included
period.key$period[!period.key$month %in% c(12,1,2,3,6,7,8,9)] <- NA

#### Multivariate ENSO Index ####
# source: http://www.esrl.noaa.gov/psd/enso/mei

mei <- read_csv("data/raw/mei.csv",col_types=cols())
# These data are for every year since 1950, and bimonthly MEI index values. 
# We'll set the months to the normal integers, keeping in mind that they represent a sliding scale

# Convert from wide to long form and add integer month identifier
mei.per <- mei %>% 
  gather(key=month.txt,value=mei,DecJan:NovDec) %>%
  arrange(Year) %>%
  mutate(month.num= rep(1:12,67)) %>%

  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("Year"="year","month.num"="month")) %>%
  filter(!is.na(period)) %>%
  

  # for each period, average the MEI value to make just one value for each period
  # so in reality, this is the 6-month averaged MEI value surrounding the monitoring period
  group_by(period) %>%
  summarise(mei.mean=mean(mei))%>%  
  # normalize to zero mean, unit variance
  mutate(mei.norm=(mei.mean-mean(mei.mean))/sd(mei.mean))

# [Not run]: Plot of mean MEI values and normalized MEI values for each period
# ggplot(mei.per,aes(x=period,y=mei.norm)) +
#   geom_line()+
#   geom_line(aes(y=mei.mean))+
#   xlab("Period")+
#   ylab("Multivariate ENSO Index")+
#   ggtitle("MEI over SNI monitoring periods")+
#   geom_hline(yintercept=0,linetype=2)

#### Pacific Decadal Oscillation ####
# source: http://research.jisao.washington.edu/pdo/

pdo <- read_csv("data/raw/PDO.csv",col_types = cols())

# Similar procedure-- convert to long form, add a month identifier, and then match to monitoring periods
pdo.per <- pdo %>%
  gather(key=month.txt,value=pdo,JAN:DEC) %>%
  arrange(YEAR) %>%
  mutate(month.num=rep(1:12,117)) %>%

  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("YEAR"="year","month.num"="month")) %>%
  filter(!is.na(period)) %>%
  
  # For each period, average the PDO value to make just one value for each period
  group_by(period) %>%
  summarise(pdo.mean=mean(pdo)) %>%
  # normalize to zero mean, unit variance
  mutate(pdo.norm=(pdo.mean-mean(pdo.mean))/sd(pdo.mean))

# [Not run]: Plot of mean PDO values and normalized PDO values for each period
# ggplot(pdo.per,aes(x=period,y=pdo.norm)) +
#   geom_line()+
#   geom_line(aes(y=pdo.mean))+
#   xlab("Period")+
#   ylab("PDO Index")+
#   ggtitle("PDO over SNI monitoring periods")+
#   geom_hline(yintercept=0,linetype=2)

#### North Pacific Gyre Oscillation ####
# source: http://www.o3d.org/npgo/

npgo <- read_csv("data/raw/npgo.csv",col_types = cols())

# NPGO is already in long form, so we don't need to convert it

# Match to periods and filter
npgo.per <- npgo %>% rename(npgo='NPGO index') %>%
  
  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("YEAR"="year","MONTH"="month")) %>%
  filter(!is.na(period)) %>%
  
  # For each period, average the NPGO value to make just one value for each period
  group_by(period) %>%
  summarise(npgo.mean=mean(npgo))%>%
  # normalize to zero mean, unit variance
  mutate(npgo.norm=(npgo.mean-mean(npgo.mean))/sd(npgo.mean))

# [Not run]: Plot of mean NPGO values and normalized NPGO values for each period
# ggplot(npgo.per,aes(x=period,y=npgo.norm)) +
#   geom_line()+
#   geom_line(aes(y=npgo.mean))+
#   xlab("Period")+
#   ylab("NPGO Index")+
#   geom_hline(yintercept=0,linetype=2)

#### Significant Wave Height ####

# Two sources of wave data
# first is buoy data from CDIP
# source: http://cdip.ucsd.edu
# Wave height and sea surface temperature collected at two nearby buoys

waves.buoy <- read_csv("data/raw/Begg_SNI_CDIP.csv",col_types = cols()) %>%

# Variable Hs denotes significant wave height in meters; 
# Derived from the zeroeth moment of the reported energy spectrum.
# Described as the "average height of the one third highest waves in the record over the time period"
  
  select(Dataset,Year,Month,Mean_Hs)

waves.per <- waves.buoy %>%
  
  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("Year"="year","Month"="month")) %>%
  filter(!is.na(period)) %>%
  
  # group by period, and find Maximum Hs during each period
  # this differs from the averaging we did for MEI, PDO, and NPGO
  group_by(period) %>%
  summarise(waves.max=max(Mean_Hs)) %>%
  
  # normalize to zero mean, unit variance
  mutate(waves.norm=(waves.max-mean(waves.max))/sd(waves.max))

# Second source is from
# http://www.sccoos.org/data/waves/
# USGS Coastal and Marine Geology Program
# http://cmgwindwave.usgsportals.net/
# Modeled waves from the Geophysical Fluid Dynamics Laboratory
# Complete, but only goes through 2005. We will join the normalized datasets together to form a more complete picture
# Code that produced the below .csv is available in this directory as "wave_height_ncdf4_extract.R"

source("data/data preparation scripts/wave_height_ncdf4_extract.R")

waves.modeled <- wavesdf %>% 
  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("year"="year","month"="month")) %>%
  filter(!is.na(period)) %>%
  
  # group by period, and find Maximum Hs during each period
  # this differs from the averaging we did for MEI, PDO, and NPGO
  group_by(period) %>%
  summarise(waves.max.mod=max(maxHs)) %>%
  
  # normalize to zero mean, unit variance
  mutate(waves.norm.mod=(waves.max.mod-mean(waves.max.mod))/sd(waves.max.mod))

# Join the two datasets together, using the normalized buoy data to "fill in" the modeled data
wavesboth <- full_join(waves.modeled,waves.per,by="period") %>%
  mutate(waves.norm.both=coalesce(waves.norm.mod,waves.norm)) %>%
  
  # normalize again and remove previous columns
  mutate(waves.norm.c=(waves.norm.both-mean(waves.norm.both))/sd(waves.norm.both))%>%
  select(-waves.norm.mod,-waves.norm,-waves.norm.both)

# [Not run]: Plot of maximum wave height values and normalized maximum weight height values for each period
# ggplot(wavesboth,aes(x=period,y=waves.norm.c)) +
#   geom_line(color="darkblue")+
#   geom_line(aes(y=waves.norm.mod),color="darkred")+
#   geom_line(aes(y=waves.norm),color="darkgreen")+
#   xlab("Period")+
#   ylab("Normalized Maximum Significant \nWave Height from two sources")+
#   ggtitle("Max Wave Height over SNI monitoring periods")+
#   geom_hline(yintercept=0,linetype=2)

#### Sea Surface Temperature ####

# Source: NOAA's Optimally Interpolated Sea Surface Temperature(https://www.ncdc.noaa.gov/oisst)
# These data were collected and analyzed using code adapted from Luke Miller
# http://lukemiller.org/index.php/2014/11/extracting-noaa-sea-surface-temperatures-with-ncdf4/
# Converted native NCDF data to .csv of daily SST, 1980-2014, for pixels near to SNI
# Code that produced the below .csv is available in this directory as "SST_ncdf4_extract.R"

source("data/data preparation scripts/SST_ncdf4_extract.R")

sst.per <- noaa.sst %>% 
  
  # pick the closest pixel of SST data
  filter(Lat==33.125,Long==240.625) %>%
  
  # Use the periods key to assign periods to each month, and remove irrelevant data
  left_join(period.key,by=c("year"="year","month"="month")) %>%
  filter(!is.na(period)) %>%
  
  # For each period, average the SST value to make just one value for each period
  group_by(period) %>%
  summarise(sst.mean=mean(mean.monthly.sst,na.rm=T)) %>%
  
  # normalize to zero mean, unit variance
  mutate(sst.norm=(sst.mean-mean(sst.mean))/sd(sst.mean))


#### Join all physical datasets ####
phys.dat <- full_join(mei.per,pdo.per,by="period") %>% full_join(npgo.per,by="period") %>% 
  full_join(wavesboth,by="period") %>% full_join(sst.per,by="period")
phys.dat.norm <- phys.dat %>% select(period, contains("norm")) %>%
  rename(mei=mei.norm,sst=sst.norm,waves=waves.norm.c,pdo=pdo.norm,npgo=npgo.norm)

## [Not run] Plot of all normalized physical variables
# gather(phys.dat.norm,key=var,value=val,mei:sst) %>%
# ggplot(aes(x=period,y=val))+
#   geom_line(aes(col=var))+
#   ggtitle("SNI Physical Variables by Monitoring Period")+
#   xlab("Period")+
#   ylab("Normalized Value")+
#   geom_hline(yintercept=0,linetype=2)

# Remove all unneeded variables from environment
rm(list=setdiff(ls(), c("phys.dat.norm","westend.norm")))
### FINISH ###
