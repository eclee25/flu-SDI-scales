# Date: 6/16/17
# functions for cleaning and import aggregation bias as the response data, where aggregation bias is composed of the fitted values from county and state surveillance models
################################################################
require(tidyverse)

################################
import_fit_aggBias_seasIntensityRR <- function(modCodeStr_cty, modCodeStr_st, filepathList){
  print(match.call())
  # import fitted values for county and state seasonal intensity models

  # import county data
  ctyDat <- import_obsFit_seasIntensityRR(modCodeStr_cty, filepathList) %>%
    rename(fit_rr_cty = fit_rr, fit_logy_cty = fit_logy) %>%
    select(season, fips, fit_rr_cty, fit_logy_cty) %>%
    mutate(fips_st = substring(fips, 1, 2))
  
  # import state data
  stDat <- import_obsFit_seasIntensityRR_st(modCodeStr_st, filepathList) %>%
    rename(fit_rr_st = fit_rr, fit_logy_st = fit_logy) %>%
    select(season, fips_st, fit_rr_st, fit_logy_st)
  
  fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
    mutate(year = season+2000) %>%
    mutate(y = fit_rr_st - fit_rr_cty) %>% # 6/20/17 same definition of "error" in Fig3 Drivers MS
    select(season, year, fips, fips_st, fit_rr_cty, fit_rr_st, y)
  
  return(fullFitDat) 
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
string_fit_fname <- function(modCodeStr){
  searchDir <-  paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/")
  return(grep("summaryStatsFitted_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}