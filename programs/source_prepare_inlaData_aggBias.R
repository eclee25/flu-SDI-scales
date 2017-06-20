## Name: Elizabeth Lee
## Date: 4/2/17
## Function: functions to prepare data for aggregation bias models
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(maptools); require(spdep)

setwd(dirname(sys.frame(1)$ofile))

#### functions for model data aggregation  ################################

################################

model11a_iliSum_v7 <- function(filepathList, modCodeStrList){
  # 6/20/17 model 11a_v7 -- total population
  # multi-season model, all sampling effort, and driver variables
  # with 2 spatial terms - state flight passenger, county neighbor
  # y = fitted cty model RR, E = fitted st model RR
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "_cc", col_names = c("st", "fips_st"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(fips_st %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  aggbias_df <- import_fit_aggBias_seasIntensityRR(modCodeStrList$cty, modCodeStrList$st, filepathList) 
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
  dummy_df <- full_join(aggbias_df, statesOnly, by = c("fips_st"))
  dummy_df1 <- full_join(dummy_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df1, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
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
    select(-adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

convert_aggBias_spatiotemporal <- function(modData_seas){
  # 10/11/16: prepare multi-season model data for aggregation bias models
  print(match.call())
  
  # grab response variable as a vector
  Y_nz <- modData_seas %>% select(y) %>% unlist
  print(Y_nz)
  
  # covariate matrix for nonzero lik: response, predictors, random effects & offset
  # 10/30/16 control flow for graph Idx # 12/20/16 graph Idx st
  if(is.null(modData_seas$graphIdx) & is.null(modData_seas$graphIdx_st)){
    Mx_nz <- modData_seas %>%
      select(dplyr::contains("X_"), dplyr::contains("O_"), fips, fips_st, regionID, ID, fit_rr_st, fit_rr_cty, season) %>%
      mutate(intercept = 1) 
  } else if(is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(dplyr::contains("X_"), dplyr::contains("O_"), fips, fips_st, regionID, ID, fit_rr_st, fit_rr_cty, season, graphIdx) %>%
      mutate(intercept = 1) 
  } else if(!is.null(modData_seas$graphIdx_st) & is.null(modData_seas$graphIdx)){
    Mx_nz <- modData_seas %>%
      select(dplyr::contains("X_"), dplyr::contains("O_"), fips, fips_st, regionID, ID, fit_rr_st, fit_rr_cty, season, graphIdx_st) %>%
      mutate(intercept = 1)
  } else{
    Mx_nz <- modData_seas %>%
      select(dplyr::contains("X_"), dplyr::contains("O_"), fips, fips_st, regionID, ID, fit_rr_st, fit_rr_cty, season, graphIdx, graphIdx_st) %>%
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