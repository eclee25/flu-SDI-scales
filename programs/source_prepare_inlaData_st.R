
## Name: Elizabeth Lee
## Date: 1/22/16
## Function: functions to create model_version data and adjacency matrix of spatial neighbors for INLA -- state scale
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(maptools); require(spdep)

#### functions for model data aggregation  ################################

model10a_iliSum_irDt_v2 <- function(filepathList){
  # 7/5/17 model 10a_v2 -- total population, iliSum irDt, scales covariates, state level
  # multi-season model, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_iliSum_irDt_shift1_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_wksToEpi_st(filepathList)
  narrAnomSpecHum_st_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_st(filepathList)
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_wksToEpi_st(filepathList)
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(narrAnomSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

model10b_iliPeak_irDt_v2 <- function(filepathList){
  # model 10b_v2 -- total population, iliPeak irDt, scales covariates, state level
  # multi-season model, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_iliPeak_irDt_shift1_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_wksToEpi_st(filepathList)
  narrAnomSpecHum_st_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_st(filepathList)
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_wksToEpi_st(filepathList)
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(narrAnomSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

model10h_iliEarly_irDt_v2 <- function(filepathList){
  # model 10h_v2 -- total population, iliEarly irDt, scales covariates, state level
  # multi-season model, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_iliEarly_irDt_shift1_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_wksToEpi_st(filepathList)
  narrAnomSpecHum_st_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_st(filepathList)
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_wksToEpi_st(filepathList)
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(narrAnomSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

model10a_iliSum_v2_db <- function(filepathList){
  # 2/14/17 model 10a_v2 -- total population, state level
  # multi-season model, shifted1 iliSum response, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = log(response+1), E = expected response+1
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_iliSum_shift1_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_st()
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_st()
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

model10f_wksToEpi_v2 <- function(filepathList){
  # 7/5/17 model 10f_v2 -- total population, state level
  # multi-season model, wks.to.epi count response in weeks, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_wksToEpi_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_wksToEpi_st(filepathList)
  narrAnomSpecHum_st_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_st(filepathList)
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_wksToEpi_st(filepathList)
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(narrAnomSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = y1) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

model10i_wksToPeak_v2 <- function(filepathList){
  # 7/2/18 model 10i_v2 -- total population, state level
  # multi-season model, wks.to.peak count response in weeks, all sampling effort, and driver variables
  # with 1 spatial terms - state flight passenger
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_st_df <- cleanR_wksToPeak_st(filepathList)
  imsCov_st_df <- cleanO_imsCoverage_st()
  imsCareseek_st_df <- cleanO_imsCareseekTot_st() # 1/5/17 visitsPerPop from sdi flu data
  # all state tables
  cpsasecIns_st_df <- cleanO_cpsasecInsured_st()
  saipePov_st_df <- cleanX_saipePoverty_st()
  censusChPop_st_df <- cleanX_censusChildPop_st()
  censusAdPop_st_df <- cleanX_censusAdultPop_st()
  ahrfHosp_st_df <- cleanX_ahrfHospitals_st()
  popDens_st_df <- cleanX_popDensity_st()
  housDens_st_df <- cleanX_housDensity_st()
  narrSpecHum_st_df <- cleanX_noaanarrSpecHum_wksToEpi_st(filepathList)
  narrAnomSpecHum_st_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_st(filepathList)
  wonderPollution_st_df <- cleanX_wonderAirParticulateMatter_wksToEpi_st(filepathList)
  acsOnePersonHH_st_df <- cleanX_acsOnePersonHH_st()
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_st(filepathList)
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  # graph index IDs
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  #### join data ####
  dummy_df <- full_join(mod_st_df, imsCov_st_df, by = c("year", "fips_st"))
  dummy_df2 <- full_join(dummy_df, cpsasecIns_st_df, by = c("year", "fips_st"))
 
  full_df <- full_join(dummy_df2, saipePov_st_df, by = c("year", "fips_st")) %>%
    full_join(imsCareseek_st_df, by = c("season", "fips_st")) %>%
    full_join(censusChPop_st_df, by = c("year", "fips_st")) %>%
    full_join(censusAdPop_st_df, by = c("year", "fips_st")) %>%
    full_join(ahrfHosp_st_df, by = c("year", "fips_st")) %>%
    full_join(popDens_st_df, by = c("year", "fips_st")) %>%
    full_join(housDens_st_df, by = c("year", "fips_st")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>% # 2/14/16 join on what?
    full_join(elderlyAnyVax_st_df, by = c("season", "abbr_st" = "st")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips_st")) %>%
    full_join(narrSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(narrAnomSpecHum_st_df, by = c("season", "fips_st")) %>%
    full_join(wonderPollution_st_df, by = c("season", "fips_st")) %>%
    full_join(acsOnePersonHH_st_df, by = c("fips_st", "year")) %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPopT)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = y1) %>% # model response y1 = log(y+1)
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips_st))
  
  return(full_df)
}
################################

convert_hurdleModel_nz_spatiotemporal_st <- function(modData_seas){
  # 10/11/16: prepare data seasonal model data for nonzero model component
  print(match.call())
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma/normal lik)
  Y_nz <- modData_seas %>% 
    select(y1) %>%
    unlist
  
  # covariate matrix for nonzero lik: response, predictors, random effects & offset
  # 10/30/16 control flow for graph Idx # 12/20/16 graph Idx st 
  # 2/14/17 didn't rm vestigial cty code
  if(is.null(modData_seas$graphIdx) & is.null(modData_seas$graphIdx_st)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips_st, regionID, ID, logE, season) %>%
      mutate(intercept = 1) 
  } else if(is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips_st, regionID, ID, logE, season) %>%
      mutate(intercept = 1) 
  } else if(!is.null(modData_seas$graphIdx_st) & is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips_st, regionID, ID, logE, season, graphIdx_st) %>%
      mutate(intercept = 1)
  } else{
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips_st, regionID, ID, logE, season, graphIdx_st) %>%
      mutate(intercept = 1)
  }
  colnames(Mx_nz) <- paste0(colnames(Mx_nz), "_nonzero")
  
  # convert matrix information to a list of lists/matrixes
  modData_seas_lists <- list()
  for (column in colnames(Mx_nz)){
    modData_seas_lists[[column]] <- Mx_nz[[column]]
  }
  # add Y response vector as a list
  modData_seas_lists[['Y']] <- Y_nz
  
  return(modData_seas_lists)
}

#### functions for shapefile manipulation ################################

read_shapefile_st <- function(filepathList){
  # read state shapefile, export state adjacency matrix to file
  print(match.call())
  print(filepathList)

  # read shapefile into R
  st.poly.full <- readShapePoly(filepathList$path_shape_st) 
  # converts polygon data to adjacency matrix
  st.adjM.export <- poly2nb(st.poly.full) 
  # exports state adjacency matrix to file
  nb2INLA(filepathList$path_adjMxExport_st, st.adjM.export) 
  
  return(st.poly.full)
}
################################

combine_shapefile_modelData_st <- function(filepathList, modelData, seasNum){
  # merge model data with poly data for a single season
  print(match.call())

  # grab state shapefile data
  st.poly.full <- read_shapefile_st(filepathList)
  st.poly.only <- attr(st.poly.full, "data") # grab only polygon data

  # merge with model data
  modelData2 <- modelData %>%
    filter(season == seasNum) %>%
    select(-state)
  modelData3 <- tbl_df(st.poly.only) %>%
    mutate(fips = as.character(STATE)) %>%
    mutate(state = tolower(as.character(NAME))) %>%
    select(-STATE, -NAME) %>%
    left_join(modelData2, by = "fips") %>%
    mutate(ID = seq_along(fips)) 
 
  return(modelData3)
}

#### test the functions here  ################################



