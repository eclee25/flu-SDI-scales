
## Name: Elizabeth Lee
## Date: 9/8/17
## Function: write reference data for region population and lat/lon coordinates

## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))

#### import data ################################
setwd('../reference_data')
# average state coordinates
state_ref <- read_csv("state_latlon.csv")

# state-region crosswalk
reg_cw <- read_csv(file="state_abbreviations_FIPS_region.csv", "c_ci", skip=1, col_names = c("fips_st", "state", "region"))
# process and merge crosswalk data: 1) drop irrelevant variables 
reg_latlon <- right_join(state_ref, reg_cw, by = c("state")) %>% 
  select(fips_st, state, region, latitude, longitude) %>%
  group_by(region) %>%
  summarise(latitude = mean(latitude), longitude = mean(longitude)) %>%
  mutate(region = paste0("R", region))

write.csv(reg_latlon, file = 'region_latlon.csv', row.names=F)
# exported 9/8/17








