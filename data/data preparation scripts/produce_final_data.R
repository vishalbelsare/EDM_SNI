### Join benthic swath and physical variables ###

## This script produces the final dataset ready for EDM analysis

## Normalized time series from benthic swath data and physical variables

library(tidyverse)

## Run the scripts that processed normalized time series
source("data/data preparation scripts/benthic_data_prep.R")
source("data/data preparation scripts/physical_data_preparation.R")

## Now we simply join the two datasets by their common monitoring period identifier
## Physical variables will be duplicated to attach to each of the benthic swath replicates

westend <- westend.norm %>% left_join(phys.dat.norm,by="period")

rm(phys.dat.norm,westend.norm)

### FINISH ###