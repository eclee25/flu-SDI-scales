
## Name: Elizabeth Lee
## Date: 1/22/16
## Function: functions to create model_version data and adjacency matrix of spatial neighbors for INLA -- county scale
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 8/30/16: move testing module to source_prepare_testing_inlaData_cty.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(maptools); require(spdep)

setwd("/home/elee/Dropbox/code")
source("return_gammaDistParams.R")
setwd(dirname(sys.frame(1)$ofile))

#### functions for model data aggregation  ################################

remove_case_exceptions <- function(full_df){
  # 1) https://www.cdc.gov/nchs/data/nvss/bridged_race/county_geography_changes.pdf: Broomfield County, Colorado (FIPS code=08014) was created effective November 15, 2001 from parts of four Colorado counties: Adams, Boulder, Jefferson, and Weld. There are estimates for this county on some, but not all, of the bridged-race files. Note that data for Broomfield County do not appear on NCHS birth or mortality files until data year 2003
  # 2) fips 48301 (Loving Cty, TX) is an outlier in all seasons
  print(match.call())
  
  full_df2 <- full_df %>%
    filter(!(fips == "08014" & season %in% 2:5)) %>% # 1
    filter(!(fips == "48301")) # 2
  
  return(full_df2)
}
################################

filter_region <- function(full_df, regionLs){
  # filter model data from a given region number
  print(match.call())
  
  full_df2 <- full_df %>%
    filter(regionID %in% regionLs)
  
  return(full_df2)
}
################################

set_region_groups <- function(regionList){
	# create dataframe with region groupings for individual region models
	print(match.call())

	regionDf <- data.frame()

	for (i in 1:length(regionList)){
		regionID <- regionList[[i]]
		regionGroup <- rep(i, length(regionID))
		regionDf <- bind_rows(regionDf, tbl_df(cbind(regionID, regionGroup)))
	}

	return(regionDf)
}
################################

model8a_iliSum_v7 <- function(filepathList){
  # 1/13/17 model 8a_v7 -- total population
  # multi-season model, shifted1 iliSum response, all sampling effort, and driver variables
  # with 2 spatial terms - state flight passenger, county neighbor
  # y1 = log(response+1), E = expected response+1
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_shift1_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  # graph index IDs
  graphIdx_df <- clean_graphIDx(filepathList, "county")
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(graphIdx_df, by = "fips") %>%
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
    select(-stateID, -adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

model8a_iliSum_regions <- function(filepathList, regionList){
  # 3/1/17 model 8a iliSum reiongs -- total population for regions, modified from 8a iliSum v7
  # multi-season model, shifted1 iliSum response, all sampling effort, and driver variables
  # with 2 spatial terms - state flight passenger, county neighbor
  # y1 = log(response+1), E = expected response+1
  print(match.call())
  print(filepathList)

  # create dataframe with region groups according to regionList specifications
  regionDf <- set_region_groups(regionList)

  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_shift1_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  # graph index IDs
  graphIdx_df <- clean_graphIDx(filepathList, "county")
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(graphIdx_df, by = "fips") %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    right_join(regionDf, by = "regionID") %>% # filter
    group_by(regionGroup) %>%
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
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(!is.na(graphIdx_st)) %>% # rm data not in graph
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-stateID, -adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

model8g_iliRate_v7 <- function(filepathList){
  # 7/6/17 model 8g_v7 -- total population
  # multi-season model, shifted1 iliRate response, all sampling effort, and driver variables
  # with 2 spatial terms - state flight passenger, county neighbor
  # y1 = log(response+1), E = expected response+1
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliRate_shift1_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  # graph index IDs
  graphIdx_df <- clean_graphIDx(filepathList, "county")
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(graphIdx_df, by = "fips") %>%
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
    select(-stateID, -adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

model8f_wksToEpi_v7 <- function(filepathList){
  # 3/31/17 model 8f_v7 -- total population
  # multi-season model, wks.to.epi count response in weeks, all sampling effort, and driver variables
  # with 2 spatial terms - state flight passenger, county neighbor
  # y1 = non-zero response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_wksToEpi_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_wksToEpi_cty(filepathList)
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_wksToEpi_cty(filepathList)
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  # graph index IDs
  graphIdx_df <- clean_graphIDx(filepathList, "county")
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(graphIdx_df, by = "fips") %>%
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
    mutate(logE = log(E), y1 = y1) %>% # model response y1 = nonzero y values
    select(-stateID, -adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

#### functions for shapefile manipulation ################################
read_shapefile_cty <- function(filepathList){
  # read & clean county shapefile
  print(match.call())
  print(filepathList)
  
  # read fips codes for continental US states only
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  # read shapefile into R
  cty.poly.full <- readShapePoly(filepathList$path_shape_cty) 
  # restrict shapefile to only US states
  cty.poly.states <- cty.poly.full[cty.poly.full@data$STATE %in% continentalOnly,]
  
  ## LOG ##
  # 6/20/16: remove GEO_IDs for Nantucket County MA (25019) & San Juan County (53055), which have no neighbors, from shapefile --> c("0500000US25019", "0500000US53055") 
  # 7/6/16: removals from shapefile need to by synced with .graph output (reference_censusCtyShapefile_oneComponent.R)
  # 7/8/16: replaced '.graph' output with ASCII file with no islands (US_county_adjacency_fips.dat) derived from true Census neighbors list where IDs are county fips codes (exported from Census/programs/reference_censusCtyAdjacency_inlaFormat.py). No longer need to remove specific GEO_IDs or keep shapefile in sync with .graph output as discussed above.
  
  return(cty.poly.states)
}
################################

combine_shapefile_modelData_cty <- function(filepathList, modelData, seasNum){
  # merge model data with poly data for a single season
  print(match.call())

  # grab state shapefile data
  cty.poly.full <- read_shapefile_cty(filepathList)
  cty.poly.only <- attr(cty.poly.full, "data") # grab only polygon data

  # merge with model data
  modelData2 <- modelData %>%
    filter(season == seasNum) 
  modelData3 <- tbl_df(cty.poly.only) %>%
    mutate(fips = paste0(as.character(STATE), as.character(COUNTY))) %>%
    select(fips, GEO_ID) %>%
    left_join(modelData2, by = "fips") %>%
    mutate(ID = seq_along(fips)) # 7/6/16: for the spatial models, the ID variable uniquely identifies the neighbors in the .graph file
  
  return(modelData3)
}
################################

convert_hurdleModel_nz_spatiotemporal <- function(modData_seas){
  # 10/11/16: prepare data seasonal model data for nonzero model component
  print(match.call())
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma/normal lik)
  Y_nz <- modData_seas %>% 
    select(y1) %>%
    unlist
  
  # covariate matrix for nonzero lik: response, predictors, random effects & offset
  # 10/30/16 control flow for graph Idx # 12/20/16 graph Idx st
  if(is.null(modData_seas$graphIdx) & is.null(modData_seas$graphIdx_st)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season) %>%
      mutate(intercept = 1) 
  } else if(is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx) %>%
      mutate(intercept = 1) 
  } else if(!is.null(modData_seas$graphIdx_st) & is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx_st) %>%
      mutate(intercept = 1)
  } else{
    Mx_nz <- modData_seas %>%
      select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx, graphIdx_st) %>%
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

################################

#### test the functions here  ################################



