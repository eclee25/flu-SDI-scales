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