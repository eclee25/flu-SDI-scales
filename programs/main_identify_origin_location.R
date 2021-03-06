## Name: Elizabeth Lee
## Date: 8/3/17
## Function: main code to identify most probable flu season source (origin) location at the county level; export dataframes for our version and Charu2017 supplementary table
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse); require(DBI); require(RMySQL) # clean_data_functions dependencies


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

#### SOURCE: clean and import response data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") 
source("source_clean_response_functions_st.R") 
source("source_identify_origin_location.R")

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)

#### MAIN - Elizabeth's version ############################

wksToEpiDat <- cleanR_wksToEpi_cty(path_list)
earlyLocDat <- subset_earliest_onset_locations_decile(wksToEpiDat) %>%
  rename(srcFips = fips)

seasons <- wksToEpiDat %>% distinct(season) %>% arrange(season) %>% unlist 
corrDat <- data.frame()

for (s in seasons){
	wksToEpiDat_seas <- wksToEpiDat %>% filter(season == s)
	earlyLocDat_seas <- earlyLocDat %>% filter(season == s)

	prepDat <- calculate_distance_from_sources(earlyLocDat_seas, wksToEpiDat_seas)

	dummyDat <- calculate_correlation_distance_onsetWeek(prepDat)
	dummyDf <- data.frame(season = s, srcID = names(dummyDat), corrCoef = t(dummyDat))
	corrDat <- tbl_df(bind_rows(corrDat, dummyDf))
}

fullDat <- left_join(earlyLocDat %>% select(srcID, srcFips), corrDat, by = "srcID")

srcLocDat_corr <- fullDat %>% group_by(season) %>% filter(corrCoef==max(corrCoef)) %>% ungroup %>% select(srcID, corrCoef)
srcLocDat_IDs <- srcLocDat_corr$srcID
srcLocDat <- earlyLocDat %>% 
  filter(srcID %in% srcLocDat_IDs) %>%
  left_join(srcLocDat_corr, by = c("srcID")) %>%
  select(season, srcFips, srcLat, srcLon, corrCoef)

write_csv(srcLocDat, "origin_locations/fluseason_source_locations_Lee.csv")

#### MAIN - Vivek's version ############################

v2 <- data.frame(season = 3:9, srcFips = c("48439", "48215", "25009", "04019", "12017", "06073", "48201"), corrCoef = c(0.41, 0.57, 0.12, 0.68, 0.33, 0.29, 0.21))
v2_IDs <- v2 %>%
  mutate(uqID = paste(season, srcFips, sep = "_")) %>%
  select(uqID) %>%
  unlist
srcLocDat_v2 <- wksToEpiDat %>%
  rename(srcFips = fips, srcLat = lat, srcLon = lon) %>%
  distinct(season, srcFips, srcLat, srcLon) %>%
  mutate(uqID = paste(season, srcFips, sep = "_")) %>%
  filter(uqID %in% v2_IDs) %>%
  left_join(v2 %>% select(-season), by = c("srcFips")) %>%
  select(season, srcFips, srcLat, srcLon, corrCoef)

write_csv(srcLocDat_v2, "origin_locations/fluseason_source_locations_Charu.csv")


