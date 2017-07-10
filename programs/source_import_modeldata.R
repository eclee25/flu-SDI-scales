## Name: Elizabeth Lee
## Date: 6/23/17
## Function: functions to import surveillance model data
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
require(data.table)

source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_response_functions_st.R") # functions to clean response (st)

#### group observed and fitted model data ################################
################################
import_obsFit_wksToEpi <- function(modCodeStr, filepathList){
  print(match.call())
  # import observed and fitted data for weeks to epidemic onset at county level
  
  # import fitted data (on the scale of y)
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_ddd_d___") %>%
    rename(fit_y = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_y, fit_sd, q_025, q_975)
  
  # import observed and expected wks to epi
  inDat <- cleanR_wksToEpi_cty(filepathList) %>%
        mutate(obs_y = y1, E = E) %>%
        select(season, fips, obs_y, E)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips")) %>%
    # mutate(obs_rr = obs_y/E, fit_rr = fit_y/E) %>%
    # mutate(cty_rr_q025 = q_025/E, cty_rr_q975 = q_975/E) %>%
    mutate(resid = (obs_y - fit_y)/fit_sd)
  
  return(obsFitDat)
}

################################
import_obsFit_wksToEpi_st <- function(modCodeStr, filepathList){
  print(match.call())
  # import observed and fitted data for weeks to epidemic onset at state level
  
  # import fitted data
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_ddd_d___") %>%
    rename(fit_y = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips_st, fit_y, fit_sd, q_025, q_975)
  
  # import observed and expected wks to epi
  inDat <- cleanR_wksToEpi_st(filepathList) %>%
    mutate(obs_y = y1, E = E) %>%
    select(season, fips_st, obs_y, E)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips_st")) %>%
    # mutate(obs_rr = obs_y/E, fit_rr = fit_y/E) %>% 
    # mutate(st_rr_q025 = q_025/E, st_rr_q975 = q_975/E) %>%
    mutate(resid = (obs_y - fit_y)/fit_sd)
  
  return(obsFitDat)
}
################################

import_obsFit_wksToEpi_ctySt <- function(modCodeStr_cty, modCodeStr_st, offset_l,filepathList){
  print(match.call())
  # import fitted values for county and state models: weeks to epidemic onset

  # import county and state data for models with offset
  if (offset_l){
    ctyDat <- import_obsFit_wksToEpi(modCodeStr_cty, filepathList) %>%
      mutate(fit_rr_cty = fit_y/E) %>%
      rename(fit_y_cty = fit_y) %>%
      rename(cty_LB = q025/E, cty_UB = q975/E) %>%
      select(season, fips, fit_rr_cty, fit_y_cty, cty_LB, cty_UB) %>%
      mutate(fips_st = substring(fips, 1, 2))
  
    stDat <- import_obsFit_wksToEpi_st(modCodeStr_st, filepathList) %>%
      mutate(fit_rr_st = fit_y/E) %>%
      rename(fit_y_st = fit_y) %>%
      rename(st_LB = q025/E, st_UB = q975/E) %>%
      select(season, fips_st, fit_rr_st, fit_y_st, st_LB, st_UB)

    fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
      mutate(fit_rrDiff_stCty = fit_rr_st-fit_rr_cty) %>%
      select(season, fips, fips_st, fit_rr_cty, fit_rr_st, fit_rrDiff_stCty, cty_LB, cty_UB, st_LB, st_UB)

  } else{ # data without offset adjustment
    ctyDat <- import_obsFit_wksToEpi(modCodeStr_cty, filepathList) %>%
      rename(fit_y_cty = fit_y) %>%
      rename(cty_LB = q025, cty_UB = q975) %>%
      select(season, fips, fit_y_cty, cty_LB, cty_UB) %>%
      mutate(fips_st = substring(fips, 1, 2))
  
    stDat <- import_obsFit_wksToEpi_st(modCodeStr_st, filepathList) %>%
      rename(fit_y_st = fit_y) %>%
      rename(st_LB = q025, st_UB = q975) %>%
      select(season, fips_st, fit_y_st, st_LB, st_UB)

    fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
      select(season, fips, fips_st, cty_LB, cty_UB, st_LB, st_UB)

  }
  
  return(fullFitDat) 
}
################################



#### paths  ################################
################################
string_fit_fname <- function(modCodeStr){
  searchDir <-  paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/")
  return(grep("summaryStatsFitted_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}
################################
string_exportFig_subsampleAggBias_folder <- function(){
  return(paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/subsampleAggBias_explore/"))
}
################################
string_exportFig_aggBias_folder <- function(){
  return(paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/aggBias_explore/"))
}

#### plotting dependencies ################################
################################
import_county_geomMap <- function(){
  print(match.call())
  
  countyMap <- map_data("county")
  data(county.fips)
  polynameSplit <- tstrsplit(county.fips$polyname, ",")
  county_geomMap <- tbl_df(county.fips) %>%
    mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
    mutate(region = polynameSplit[[1]]) %>%
    mutate(subregion = polynameSplit[[2]]) %>%
    full_join(countyMap, by = c("region", "subregion")) %>%
    filter(!is.na(polyname) & !is.na(long)) %>%
    rename(state = region, county = subregion) %>%
    rename(region = fips) %>%
    select(-polyname)
  
  return(county_geomMap)
}

#### obsolete functions? ################################
################################
import_obsFit_seasIntensityRR <- function(modCodeStr, filepathList){
  print(match.call())
  
  # import fitted data (on the scale of log(y))
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_logy = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_logy, fit_sd)
  
  # import observed and expected log seasIntensity (shift1)
  if (grepl("2009p", modCodeStr)){
    inDat <- cleanR_iliSum_2009p_shift1_cty(filepathList) %>%
        mutate(obs_logy = log(y1), logE = log(E)) %>%
        mutate(season = 10) %>%
        select(season, fips, obs_logy, logE)
  } else{
    inDat <- cleanR_iliSum_shift1_cty(filepathList) %>%
        mutate(obs_logy = log(y1), logE = log(E)) %>%
        select(season, fips, obs_logy, logE)
  }
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips")) %>%
    mutate(obs_rr = obs_logy-logE, fit_rr = fit_logy-logE) %>%
    mutate(resid = (obs_logy - fit_logy)/fit_sd)
  
  return(obsFitDat)
}

################################
import_obsFit_seasIntensityRR_st <- function(modCodeStr, filepathList){
  print(match.call())
  # import observed and fitted data for seasonal intensity state models
  
  # import fitted data
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_logy = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips_st, fit_logy, fit_sd)
  
  # import observed data
  inDat <- cleanR_iliSum_shift1_st(filepathList) %>%
    mutate(obs_logy = log(y1), logE = log(E)) %>%
    select(season, fips_st, obs_logy, logE)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips_st")) %>%
    mutate(obs_rr = obs_logy-logE, fit_rr = fit_logy-logE) %>%
    mutate(resid = (obs_logy - fit_logy)/fit_sd)
  
  return(obsFitDat)
}