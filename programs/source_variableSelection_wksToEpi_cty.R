
## Name: Elizabeth Lee
## Date: 8/9/17
## Function: functions to perform EDA for variable selection at cty level
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse)
# source("source_clean_data_functions.R")

#### functions for data aggregation  ################################

prepare_allCov_wksToEpi_cty <- function(filepathList){
  # wksToEpi response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_wksToEpi_cty(filepathList) 
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() 
  imsCareseekChild_cty_df <- cleanO_imsCareseekChild_cty() 
  imsCareseekAdult_cty_df <- cleanO_imsCareseekAdult_cty() 
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  saipeInc_cty_df <- cleanX_saipeIncome_cty()
  ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
  ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
  censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
  brfssMedCost_cty_df <- cleanX_brfssMedCost_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_prep <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  narrAnomSpecHum_cty_df <- cleanX_noaanarrAnomSpecHum_wksToEpi_cty(filepathList)
  narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  cbpSocialAssoc_cty_df <- cleanX_cbpSocialAssoc_cty()
  acsOneParentHH_cty_df <- cleanX_acsOneParentFamHH_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  acsAvgHHSize_cty_df <- cleanX_acsAvgHHSize_cty()
  brfssPoorHealth_cty_df <- cleanX_brfssPoorHealth_cty()
  brfssUnhealthyDays_cty_df <- cleanX_brfssUnhealthyDays_cty()
  sourceLocDist_cty_df <- cleanX_distanceFromSourceLocations_cty(filepathList)
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  cdcH3A_df <- cleanX_cdcFluview_H3A_region() %>% select(-region)
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
    full_join(imsCareseekChild_cty_df, by = c("season", "fips")) %>%
    full_join(imsCareseekAdult_cty_df, by = c("season", "fips")) %>%
    full_join(saipeInc_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaid_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcare_cty_df, by = c("year", "fips")) %>%
    full_join(censusInfTodPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusEldPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfPhys_cty_df, by = c("year", "fips")) %>%
    full_join(brfssMedCost_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_prep, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcFluPos_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrAnomSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(cbpSocialAssoc_cty_df, by = c("fips", "year")) %>%
    full_join(acsOneParentHH_cty_df, by = c("fips", "year")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(sourceLocDist_cty_df, by = c("fips", "season")) %>%
    full_join(acsAvgHHSize_cty_df, by = c("fips", "year")) %>%
    full_join(brfssPoorHealth_cty_df, by = c("fips", "year")) %>%
    full_join(brfssUnhealthyDays_cty_df, by = c("fips", "year")) %>%
    full_join(graphIdx_df, by = "fips") %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    # group_by(season) %>% # 1/13/17 data fxn for multi-season models
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseekT = centerStandardize(visitsPerPopT)) %>%
    mutate(O_careseekC = centerStandardize(visitsPerPopC)) %>%
    mutate(O_careseekA = centerStandardize(visitsPerPopA)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_mcaid = centerStandardize(mcaidElig)) %>%
    mutate(X_mcare = centerStandardize(mcareElig)) %>%
    mutate(X_infantToddler = centerStandardize(infantToddler)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_elderly = centerStandardize(elderly)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_medcost = centerStandardize(medcost)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_anomHumidity = centerStandardize(anomHumidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_socialOrgs = centerStandardize(socialOrgs)) %>%
    mutate(X_singleParentHH = centerStandardize(prop_oneParentFamHH)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_avgHHSize = centerStandardize(avgHHSize)) %>%
    mutate(X_poorHealth = centerStandardize(poorhealth)) %>%
    mutate(X_unhealthyDays = centerStandardize(unhealthydays)) %>%
    mutate(X_latitude = centerStandardize(lat)) %>%
    mutate(X_sourceLocDist = centerStandardize(sourceLocDist)) %>%
    # ungroup %>% # 1/13/17 data fxn for multi-season models
    filter(fips_st %in% continentalOnly) %>%
    mutate(logE = log(E), logy = log(y1)) %>%
    select(-adjProviderCoverage, -visitsPerPopT, -visitsPerPopC, -visitsPerPopA, -insured, -poverty, -income, -mcaidElig, -mcareElig, -infantToddler, -child, -adult, -elderly, -hospitalAccess, -physicianAccess, -medcost, -popDensity, -housDensity, -commutInflows_prep, -pass, -fluPos, -H3, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -anomHumidity, -temperature, -avg_pm, -socialOrgs, -prop_oneParentFamHH, -perc_hh_1p, -avgHHSize, -poorhealth, -unhealthydays, -sourceLocDist) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))

    return(full_df)
}

################################

#### test the functions here  ################################

# test <- model_singleVariable_inla_st(allDat2, "iliSum", 2, "X_poverty")
