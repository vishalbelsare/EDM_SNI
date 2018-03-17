# Benthic swath data preparation
# Data of benthic densities of key species of interest for EDM

# Load required libraries
library(tidyverse)
library(lubridate)

# read in raw integrated quad swath data directly from source; https://environmentaldatainitiative.org/
dat <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.6.1&entityid=6c16f3b59574b044dd3a44cef29b9b9b",
                col_types = "cccccDciidiccccdd")

# filter out just the sampling site of interest at San Nicolas Island
sites <- dat %>%
  filter(data_source=="sni")%>%
  distinct(site_id,subsite_id,.keep_all = T)%>%
  select(data_source,site_id,subsite_id,site_name,longitude,latitude)
westend <- dat %>%
  filter(data_source=="sni",site_name==12)

# filter for species of interest
spp <- c("Mesocentrotus franciscanus","Strongylocentrotus purpuratus","Macrocystis pyrifera","Laminaria","Pterygophora californica")
westend <- westend %>%
  filter(taxon_name %in% spp) %>%
  # short-hand names for species for later reference
  mutate(spp = case_when(
    taxon_name=="Mesocentrotus franciscanus" ~ "red",
    taxon_name=="Strongylocentrotus purpuratus" ~ "purp",
    # CHECK THAT MAC AND YOUNG MAC ARE CORRECT (ASK ROBERT MILLER)
    taxon_name=="Macrocystis pyrifera" & proj_taxon_id=="t-s-068" ~ "mac",
    taxon_name=="Macrocystis pyrifera" & proj_taxon_id=="t-s-080" ~ "ymac",
    taxon_name=="Laminaria" ~ "lam",
    taxon_name=="Pterygophora californica" ~ "pter"
  ))%>%
  arrange(taxon_name,date) %>%
  # calculate density
  mutate(dens=count/area) %>%
  # remove unneeded variables
  select(date,proj_taxon_id,auth_taxon_id,taxon_name,spp,transect_id,replicate_id,area,count,dens)

# we need to apply distinct time periods to the data, because surveys happen in Fall and Spring, but each survey occurs 
# over the course of a few days.
# for example, for our analysis, the date October 19, 1987 should be in the same "monitoring period" as October 20 or October 21, 1987.
# if observations are within 50 days of each other, we consider them the same monitoring period

# To accomplish this standardization,
# this function takes a vector of difference in days between successive observations. If the difference is less than 50 days, assign to
# same monitoring period as the previous obsrevation, otherwise count up by 1 period. If difference is greater than 250, count up by 2. 
# If difference is greater than 400 days, count up by 3 (because this would imply a gap of at least a year and a half, 
# so at least two "periods" are missing,e.g. Fall 2011 to Spring 2013 skips two monitoring periods)

assign_period <- function(diffdays) {
  p <- 1
  out <- numeric(length(diffdays))
  for(i in 1:length(diffdays)) {
    if(is.na(diffdays[i]) | diffdays[i]<50) {out[i] <- p}
    else if(diffdays[i]>=50 & diffdays[i]<250) {p<-p+1;out[i]<-p}
    else if(diffdays[i]>=250 & diffdays[i]<400) {p<-p+2;out[i]<-p}
    else {p<-p+3;out[i]<-p}
  }
  out
}

westend.norm <- westend %>%
  # Group by species
  group_by(proj_taxon_id) %>%
  # calculate vector of difference (in days) between observations
  mutate(diffdays=as.numeric(difftime(date,lag(date,1),units="days")),
         
         # assign periods with function above
         period=assign_period(diffdays))%>%
  
  # remove diffdays, don't need it anymore
  select(-diffdays)%>%
  
  # For EDM we need normalized time series for each species (zero mean, unit variance)
  # Group by species and permanent transect ID, then normalize across each species/transect time series
  # after taking a mean of the replicates in each year
  group_by(spp,transect_id,period)%>%
  summarise(dens=mean(dens))%>%
  mutate(norm=(dens-mean(dens,na.rm=T))/sd(dens,na.rm=T))%>%
  ungroup()%>%
  # finally, spread to wideform data (period in rows, species in columns)
  select(-dens)%>%
  spread(spp,norm) %>%
  
  # back-fill empty periods so we don't cross time periods
  group_by(transect_id) %>% complete(period=full_seq(x=period,1))%>%
  ungroup()

# Remove all unneeded variables from environment
rm(dat,sites,westend)

### FINISH ###