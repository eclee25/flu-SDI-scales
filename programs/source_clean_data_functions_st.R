## Name: Elizabeth Lee
## Date: 2/13/17
## Function: Functions for cleaning response and covariate data for INLA at state level
## Filenames: 
## Data Source: 
## Notes:
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### functions for model data cleaning ################################
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

##### SAMPLING EFFORT DATA ##########################################
cleanO_imsCoverage_st <- function(){
  # clean IMS Health adjusted physician coverage (database coverage)
  # center and standardize the transformed variables
  print(match.call())
  
  # import physician coverage data
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "IMS_physicianCoverage_st")
  # sel.statement <- "Select * from IMS_physicianCoverage_st limit 5"
  sel.statement <- "SELECT year, fips_st, adjProviderCoverage, sampViz, sampProv FROM IMS_physicianCoverage_st"
  dummy <- dbGetQuery(con, sel.statement)
  
  # clean coverage data
  covDat <- dummy %>%
  	select(fips_st, year, adjProviderCoverage) %>%
  	arrange(fips_st, year)
  return(covDat)
  
}
################################

cleanO_cpsasecInsured_st <- function(){
  # clean CPS-ASEC insured data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "HI_CPSasec_state")
  # sel.statement <- "Select * from HI_CPSasec_state limit 5"
  sel.statement <- "SELECT year, state_id AS fips_st, Percent_covered/100 AS insured_prop FROM HI_CPSasec_state WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
 
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(insured = insured_prop) %>%
    select(fips_st, year, insured)
  
  return(output)
}
################################

cleanO_imsCareseekTot_st <- function(){
  # 2/13/17: clean IMS Health visits per population for total pop at state level exported from mysql
  print(match.call())
  
  # import visit data
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-sdi")
  dbListTables(con)
  
  dbListFields(con, "flu")
  # sel.statement <- "Select * from flu limit 5"
  sel.statement <- "SELECT WEEK AS week, PATIENT_ZIP3 AS zip3, sum(ANY_DIAG_VISIT_CT) AS visits FROM flu WHERE SERVICE_PLACE = 'TOTAL' AND patient_zip3 <> 'TOT' AND (MONTH(week) <= 4 OR MONTH(week) >= 11) AND AGEGROUP = 'TOTAL' GROUP BY WEEK, PATIENT_ZIP3"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  # import total pop data at state level
  popDat <- clean_pop_st_plain()
  
  # spatial crosswalk: fips_st, zip3, abbr_st
  cw <- cw_zip3_st() %>% select(-abbr_st)
  
  vizZip3 <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substring(week, 3, 4))) %>%
    mutate(season = ifelse(as.numeric(substring(week, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(zip3, season) %>%
    summarise(visits = sum(visits, na.rm = TRUE)) %>% 
    ungroup %>%
    arrange(season, zip3) %>%
    filter(season >= 3 & season <= 9) 
  
  # clean zip3 visits to state visits
  vizSt <- vizZip3 %>% 
    full_join(cw, by = c("zip3")) %>%
    group_by(fips_st, season) %>%
    summarise(visits = sum(visits, na.rm = TRUE)) %>%
    ungroup %>%
    filter(!is.na(fips_st)) %>%
    mutate(visits = ifelse(is.na(visits), 0, visits)) %>%
    mutate(year = season + 2000)

  # combine viz and pop dat
  output <- vizSt %>%
    full_join(popDat, by = c("year", "fips_st")) %>%
    mutate(visitsPerPopT = visits/pop) %>%
    filter(season >= 3 & season <= 9) %>%
    select(fips_st, season, visitsPerPopT) %>%
    arrange(fips_st, season)
  
  return(output)
}

##### DRIVER DATA ##########################################

##### social determinants ##########
cleanX_saipePoverty_st <- function(){
  # clean SAIPE percentage of population in poverty state data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_poverty")
  # sel.statement <- "SELECT * from SAIPE_poverty limit 5"
  sel.statement <- "SELECT year, state_id AS fips_st, all_poverty_percent/100 AS inPoverty_prop FROM SAIPE_poverty WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(poverty = inPoverty_prop) %>%
    select(fips_st, year, poverty) 
  
  return(output)
}
################################

cleanX_saipeIncome_st <- function(){
  # clean SAIPE median household income state data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_income")
  # sel.statement <- "SELECT * from SAIPE_income limit 5"
  sel.statement <- "SELECT year, state_id AS fips_st, med_income as medianIncome FROM SAIPE_income WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(income = medianIncome) %>%
    select(fips_st, year, income) 
  
  return(output)
}
################################

cleanX_ahrfMedicaidEligibles_st <- function(){
  # clean AHRF Medicaid eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, (mcaid_child + mcaid_adult) AS mcaidEligTot, population as pop FROM access_medicare_medicaid WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    group_by(fips_st, year) %>%
    summarise(mcaidEligTot = sum(mcaidEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcaidEligTot = ifelse(mcaidEligTot == 0, NA, mcaidEligTot)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(mcaidElig = mcaidEligTot/pop) %>%
    filter(year %in% 2004:2008) %>% # 3/2/16: 2005 mcaidElig is NA if years before 2004 are included; not sure why
    select(fips_st, year, mcaidElig) 
  
  return(output)
}
################################

##### demography ##########

cleanX_ahrfMedicareEligibles_st <- function(){
  # clean AHRF Medicare eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, mdcr_elig AS mcareEligTot, population AS pop FROM access_medicare_medicaid"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    group_by(fips_st, year) %>%
    summarise(mcareEligTot = sum(mcareEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcareEligTot = ifelse(mcareEligTot == 0, NA, mcareEligTot)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(mcareElig = mcareEligTot/pop) %>%
    select(fips_st, year, mcareElig) 
  
  return(output)
}
################################

cleanX_censusInfantPop_st <- function(){
  # clean Census population data for <=2 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips as fips_st, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'infant' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(infant = infant/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusToddlerPop_st <- function(){
  # clean Census population data for 3-4 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement.toddler <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement.toddler <- "SELECT year, fips as fips_st, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'toddler' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement.toddler)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(toddler = toddler/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusChildPop_st <- function(){
  # clean Census population data for 5-19 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement.child <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement.child <- "SELECT year, fips as fips_st, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'child' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement.child)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(child = child/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusAdultPop_st <- function(){
  # clean Census population data for 20-64 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips as fips_st, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'adult' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(adult = adult/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusElderlyPop_st <- function(){
  # clean Census population data for 65+ yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips as fips_st, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'elderly' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(elderly = elderly/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_ahrfHospitals_st <- function(){
  # clean AHRF hospitals per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, hosp, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    group_by(fips_st, year) %>%
    summarise(hospCt = sum(hosp, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(hospCt = ifelse(hospCt == 0, NA, hospCt)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(hospitalAccess = hospCt/pop) %>%
    select(fips_st, year, hospitalAccess) 
  
  return(output)
}
################################
cleanX_ahrfPhysicians_st <- function(){
  # clean AHRF physicians per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, physicians, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    group_by(fips_st, year) %>%
    summarise(physCt = sum(physicians, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(physCt = ifelse(physCt == 0, NA, physCt)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(physicianAccess = physCt/pop) %>%
    select(fips_st, year, physicianAccess) 
  
  return(output)
}
################################

##### contact/travel patterns ##########

cleanX_popDensity_st <- function(){
  # clean population density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips AS fips_cty, popDens_land FROM Census_popdensity_county WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    rename(popDensity = popDens_land) %>%
    select(fips_st, year, popDensity) %>%
    arrange(fips_st, year)
  
  return(output)
}
################################

cleanX_housDensity_st <- function(){
  # clean housing unit density per land area data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips AS fips_cty, popDens_housing FROM Census_popdensity_county WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips_st = substring(fips_cty, 1, 2)) %>%
    rename(housDensity = popDens_housing) %>%
    select(fips_st, year, housDensity) %>%
    arrange(fips_st, year)
  
  return(output)
}
################################

cleanX_acsCommutInflows_st <- function(){
  # clean out-of-state commuters per population entering the state
  # will need to calculate incoming commuters per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_ACS0610_iconv")
  # sel.statement <- "SELECT * from transport_ACS0610_iconv limit 5"
  sel.statement <- "SELECT state_id_residence_2digit, county_id_residence_3digit, state_id_workplace_3digit, state_workplace, Number FROM transport_ACS0610_iconv"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(domesticWork = ifelse(substring(state_id_workplace_3digit, 1, 1) == "0", TRUE, FALSE)) %>%
    filter(domesticWork) %>%
    mutate(fips_wrk = substr.Right(state_id_workplace_3digit, 2)) %>%
    rename(fips_res = state_id_residence_2digit) %>%
    filter(fips_res != fips_wrk) %>%
    select(-state_id_workplace_3digit, -domesticWork) %>%
    group_by(fips_wrk) %>%
    summarise(ct_2006 = sum(Number)) %>%
    mutate(ct_2007 = ct_2006, ct_2008 = ct_2006, ct_2009 = ct_2006) %>%
    gather(year, commutInflows_prep, ct_2006:ct_2009, convert = TRUE) %>%
    mutate(year = as.numeric(substr.Right(year, 4))) %>%
    ungroup %>%
    select(fips_wrk, year, commutInflows_prep)
  
  return(output)
}
################################

cleanX_btsPassInflows_st <- function(){
  # clean flight passengers per population entering the state on average during flu months
  # will need to calculate incoming passengers per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_BTS0014_T100D_Market_All_Carrier")
  # sel.statement <- "SELECT * from transport_BTS0014_T100D_Market_All_Carrier limit 5"
  sel.statement <- "SELECT PASSENGERS, ORIGIN, ORIGIN_STATE_FIPS, DEST, DEST_STATE_FIPS, YEAR, MONTH from transport_BTS0014_T100D_Market_All_Carrier where (PASSENGERS > 0 and (MONTH <= 4 or MONTH >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
 
  output <- tbl_df(dummy) %>%
    rename(pass = PASSENGERS, fips_origin = ORIGIN_STATE_FIPS, fips_dest = DEST_STATE_FIPS, year = YEAR, month = MONTH) %>%
    filter(!(year == 2001 & month <= 4)) %>%
    filter(!(year == 2009 & month >= 11)) %>%
    filter(fips_origin != fips_dest) %>%
    group_by(fips_dest, year, month) %>%
    summarise(pass = sum(pass, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(season = ifelse(month <= 4, as.integer(substr.Right(year, 2)), as.integer(substr.Right(year, 2))+1)) %>%
    group_by(fips_dest, season) %>%
    summarise(pass_prep = mean(pass, na.rm = TRUE)) %>% 
    ungroup %>%
    select(season, fips_dest, pass_prep)
  
  return(output)
}
################################

##### environmental factors ##########

cleanX_noaanarrSpecHum_st <- function(){
  # clean average specific humidity near population-weighted centroid of the state during flu months (daily)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_specHum_state")
  # sel.statement <- "SELECT * from env_NOAANARR_specHum_state limit 5"
  sel.statement <- "SELECT fips as fips_st, year, date as dayDate, humidity from env_NOAANARR_specHum_state where (MONTH(date) <= 4 or MONTH(date) >= 11)"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips_st, season) %>%
    summarise(humidity = mean(humidity, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}
################################

cleanX_noaanarrSfcTemp_st <- function(){
  # clean average surface temperature near population-weighted centroid of the state during flu months (daily, Kelvin)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_sfcTemp_state")
  # sel.statement <- "SELECT * from env_NOAANARR_sfcTemp_state limit 5"
  sel.statement <- "SELECT fips as fips_st, year, date as dayDate, temperature from env_NOAANARR_sfcTemp_state where (MONTH(date) <= 4 or MONTH(date) >= 11)"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips_st, season) %>%
    summarise(temperature = mean(temperature, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}
################################

cleanX_wonderAirParticulateMatter_st <- function(){
  # clean data on fine particulate matter (air pollution) with aerodynamic diameter < 2.5 micrometers by state, which was aggregated from 10 km square grids (monthly, micrograms/meter^3); monthly data are averages of daily observations
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "airpollution_wonder0311_state")
  # sel.statement <- "SELECT * from airpollution_wonder0311_state limit 5"
  sel.statement <- "SELECT fips_st, season, month, avg_pm from airpollution_wonder0311_state where month <= 4 or month >= 11"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    group_by(fips_st, season) %>%
    summarise(avg_pm = mean(avg_pm, na.rm = TRUE)) %>%
    ungroup %>%
    arrange(fips_st, season)
  
  return(output)
  
}

##### social cohesion ##########
################################

cleanX_acsOnePersonHH_st <- function(){
  # clean ACS data on single person households, state-level data available with 1-year estimates
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "socialcohesion_onePersonHH_ACS0515_1yr_state")
  # sel.statement <- "SELECT * from socialcohesion_onePersonHH_ACS0515_1yr_state limit 5"
  sel.statement <- "SELECT fips_st, year, perc_hh_1p from socialcohesion_onePersonHH_ACS0515_1yr_state"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  dummy2 <- tbl_df(dummy) %>% 
    select(fips_st, year, perc_hh_1p)
  
  # copy 2005 data for 2003 & 2004 entries
  dupDat <- dummy2 %>%
    filter(year == 2005)
  output <- bind_rows(dupDat %>% mutate(year = 2003), dupDat %>% mutate(year = 2004), dummy2) %>%
    fillValues_years("perc_hh_1p") %>%
    arrange(fips_st, year)
  
  return(output)
  
}

# #### testing area ################################
# dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
# 
# setwd(dirname(sys.frame(1)$ofile))
# setwd('../reference_data')
# path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
# path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
# setwd("../R_export")
# path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
# 
# # put all paths in a list to pass them around in functions
# path_list <- list(path_abbr_st = path_abbr_st,
#                   path_latlon_cty = path_latlon_cty,
#                   path_response_cty = path_response_cty)
# setwd(dirname(sys.frame(1)$ofile))
# source("source_clean_data_functions.R")
# 
# all state tables
# cov_df <- cleanO_imsCoverage_st()
# cs_df <- cleanO_imsCareseekTot_st()
# cpsasecInsured_df <- cleanO_cpsasecInsured_st()
# saipePoverty_df <- cleanX_saipePoverty_st()
# saipeIncome_df <- cleanX_saipeIncome_st() 
# ahrfMcaid_df <- cleanX_ahrfMedicaidEligibles_st()
# ahrfMcare_df <- cleanX_ahrfMedicareEligibles_st()
# censusInfPop_df <- cleanX_censusInfantPop_st()
# censusTodPop_df <- cleanX_censusToddlerPop_st()
# censusChPop_df <- cleanX_censusChildPop_st()
# censusAdPop_df <- cleanX_censusAdultPop_st()
# censusEldPop_df <- cleanX_censusElderlyPop_st()
# ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
# ahrfPhys_df <- cleanX_ahrfPhysicians_st()
# popDens_df <- cleanX_popDensity_st()
# housDens_df <- cleanX_housDensity_st()
# acsCommut_prep <- cleanX_acsCommutInflows_st()
# btsPass_prep <- cleanX_btsPassInflows_st()
# narrSpecHum_df <- cleanX_noaanarrSpecHum_st()
# narrSfc_df <- cleanX_noaanarrSfcTemp_st()
# pollution_df <- cleanX_wonderAirParticulateMatter_st()
# onePersonHH_df <- cleanX_acsOnePersonHH_st()
