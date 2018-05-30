## Name: Elizabeth Lee
## Date: 5/29/18
## Function: functions to import surveillance model data
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
require(data.table)

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_response_functions_st.R") # functions to clean response (st)
source("source_clean_response_functions_reg.R") # functions to clean response (reg)


#### iliEarly ################################
import_obs_iliEarly <- function(filepathList){
  print(match.call())
  # import observed data for ili in early flu season at county level
  # acts as a wrapper for cleanR_iliEarly_irDt_shift1_cty
  
  # import observed and expected ili in early flu season
  inDat <- cleanR_iliEarly_irDt_shift1_cty(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, fips, obs_y, E, pop) 
  # add lat/lon coords
  coordDat <- read_csv(filepathList$path_latlon_cty, col_types = "_c__dd")
  obsDat <- left_join(inDat, coordDat, by = c("fips")) %>%
    filter(!(substring(fips, 1, 2) %in% c("02", "15")))
  
  return(obsDat)
}
################################
import_obs_iliEarly_st <- function(filepathList){
  print(match.call())
  # import observed data for ili in early flu season at state level
  # acts as a wrapper for cleanR_iliEarly_irDt_shift1_st
  
  # import observed and expected ili peak
  obsDat <- cleanR_iliEarly_irDt_shift1_st(filepathList) %>%
    mutate(obs_y = y1, E = E) %>%
    select(season, fips_st, obs_y, E, pop) %>%
    filter(!(fips_st %in% c("02", "15")))
  
  return(obsDat)
}
################################
import_obs_iliEarly_reg <- function(filepathList){
    print(match.call())
    # import observed data for ili in early season at region level (no region level models)
    # acts as a wrapper for cleanR_iliEarly_shift1_reg

    # import observed and expected ili in early season
    obsDat <- cleanR_iliEarly_irDt_shift1_reg(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, regionID, obs_y, E, pop)
    
    return(obsDat)
}

#### iliPeak ################################
import_obs_iliPeak <- function(filepathList){
  print(match.call())
  # import observed data for peak ili at county level
  # acts as a wrapper for cleanR_iliPeak_irDt_shift1_cty
  
  # import observed and expected ili in early flu season
  inDat <- cleanR_iliPeak_irDt_shift1_cty(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, fips, obs_y, E, pop)
  # add lat/lon coords
  coordDat <- read_csv(filepathList$path_latlon_cty, col_types = "_c__dd")
  obsDat <- left_join(inDat, coordDat, by = c("fips")) %>%
    filter(!(substring(fips, 1, 2) %in% c("02", "15")))
  
  return(obsDat)
}
################################
import_obs_iliPeak_st <- function(filepathList){
  print(match.call())
  # import observed data for ili peak at state level
  # acts as a wrapper for cleanR_iliPeak_irDt_shift1_st
  
  # import observed and expected ili peak
  obsDat <- cleanR_iliPeak_irDt_shift1_st(filepathList) %>%
    mutate(obs_y = y1, E = E) %>%
    select(season, fips_st, obs_y, E, pop) %>%
    filter(!(fips_st %in% c("02", "15")))
  
  return(obsDat)
}
################################
import_obs_iliPeak_reg <- function(filepathList){
    print(match.call())
    # import observed data for peak ili at region level (no region level models)
    # acts as a wrapper for cleanR_iliPeak_shift1_reg

    # import observed and expected peak ili
    obsDat <- cleanR_iliPeak_irDt_shift1_reg(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, regionID, obs_y, E, pop)
    
    return(obsDat)
}

#### iliSum ################################
import_obs_iliSum <- function(filepathList){
  print(match.call())
  # import observed data for ili for total flu season at county level
  # acts as a wrapper for cleanR_iliSum_irDt_shift1_cty
  
  # import observed and expected ili for total flu season
  inDat <- cleanR_iliSum_irDt_shift1_cty(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, fips, obs_y, E, pop)
  # add lat/lon coords
  coordDat <- read_csv(filepathList$path_latlon_cty, col_types = "_c__dd")
  obsDat <- left_join(inDat, coordDat, by = c("fips")) %>%
    filter(!(substring(fips, 1, 2) %in% c("02", "15")))
  
  return(obsDat)
}
################################
import_obs_iliSum_st <- function(filepathList){
  print(match.call())
  # import observed data for ili for total flu season at state level
  # acts as a wrapper for cleanR_iliSum_irDt_shift1_st
  
  # import observed and expected ili for total flu season
  obsDat <- cleanR_iliSum_irDt_shift1_st(filepathList) %>%
    mutate(obs_y = y1, E = E) %>%
    select(season, fips_st, obs_y, E, pop) %>%
    filter(!(fips_st %in% c("02", "15")))
  
  return(obsDat)
}

################################

#### data processing ################################
################################


#### paths  ################################
################################


#### plotting dependencies ################################
################################
