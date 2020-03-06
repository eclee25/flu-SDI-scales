## Name: Elizabeth Lee
## Date: 8/30/17
## Function: functions to perform OR (2x2 contingency table) single predictor exploration for aggregation bias. See 8/30/17 lab notes for more detail.
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse); require(data.table); require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))

source("source_clean_data_functions.R")
source("source_clean_data_functions_st.R")
source("source_clean_response_functions_cty.R")
source("source_clean_response_functions_st.R")
source("source_prepare_inlaData_cty.R")
source("source_prepare_inlaData_st.R")
source("source_export_inlaModelComparisons.R") # overlapping_intervals, import_fit_posteriorSamples

#### import aggBias data ################################
################################
import_fit_aggBias_wksToEpi <- function(modCodeStr_cty, modCodeStr_st, filepathList){
  print(match.call())
  # import fitted values for county and state seasonal intensity models

  # import county data
  ctyDat <- import_obsFit_wksToEpi(modCodeStr_cty, filepathList) %>%
    rename(fit_y_cty = fit_y) %>%
    select(season, fips, fit_y_cty) %>%
    mutate(fips_st = substring(fips, 1, 2))
  
  # import state data
  stDat <- import_obsFit_wksToEpi_st(modCodeStr_st, filepathList) %>%
    rename(fit_y_st = fit_y) %>%
    select(season, fips_st, fit_y_st)
  
  fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
    mutate(year = season+2000) %>%
    mutate(bias = fit_y_st - fit_y_cty) %>% # st-cty so positive values are overestimates
    mutate(biasMag = abs(bias)) %>%
    select(season, year, fips, fips_st, fit_y_cty, fit_y_st, bias, biasMag)
  
  return(fullFitDat) 
}
################################
import_obsFit_wksToEpi_st <- function(modCodeStr, filepathList){
  print(match.call())
  # import observed and fitted data for seasonal onset state models

  # import fitted data
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_y = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips_st, fit_y, fit_sd)

  # import observed data
  inDat <- cleanR_wksToEpi_st(filepathList) %>%
    rename(obs_y = y1) %>%
    select(season, fips_st, obs_y)
    
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips_st")) 

    return(obsFitDat)
}
################################
import_obsFit_wksToEpi <- function(modCodeStr, filepathList){
  print(match.call())

  print(string_fit_fname(modCodeStr))

  # import fitted data (on the scale of log(y))
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_y = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_y, fit_sd)

  # import observed and expected wksToEpi
  inDat <- cleanR_wksToEpi_cty(filepathList) %>%
        rename(obs_y = y1) %>%
        select(season, fips, obs_y)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips")) 
  
  return(obsFitDat)
}
################################
import_fit_posteriorSamples <- function(modCode){
  print(match.call())
  # import raw posterior samples, N.B. that everything is on the log scale (posterior predictive and model coefficients). That is, centered values are 1 and everything needs to be exponentiated.

  allSamplesDat <- read_csv(string_fitSamples_fname(modCode))
  fitSamplesDat <- allSamplesDat %>%
    select(contains("Predictor"))

  # import ids from summaryStats
  ids <- import_fit_summaryStats(modCode) %>%
    select(season, contains("fips"))

  gatherDat <- fitSamplesDat %>%
    mutate(sampleNum = 1:n()) %>%
    gather(predictor, logsample, contains("Predictor")) %>%
    mutate(sample = exp(logsample))  # samples are on log scale due to poiss lik
  # cols: sampleNum, predictor, logsample, sample
    
  uqPredictorNames <- gatherDat %>% 
    distinct(predictor)

  stopifnot(nrow(ids) == nrow(uqPredictorNames)) # check for bind_cols
  # different processing and column names for state and county level samples
  if(is.null(ids$fips)){
    summaryDat <- bind_cols(ids, uqPredictorNames) %>%
        full_join(gatherDat, by = c("predictor")) %>%
        rename(sample_st = sample) %>%
        select(season, fips_st, sampleNum, sample_st)
  } else{
    summaryDat <- bind_cols(ids, uqPredictorNames) %>%
        full_join(gatherDat, by = c("predictor")) %>%
        mutate(fips_st = substring(fips, 1, 2)) %>%
        rename(sample_cty = sample) %>%
        select(season, fips, fips_st, sampleNum, sample_cty)
  }  

  return(summaryDat)
}
################################
import_fit_posteriorSamples_aggBias_2x2 <- function(modCodeLs){
    print(match.call())
    # calculate aggregation bias between county and state fitted posterior samples
    # fullDat cols: season, fips, fips_st, sampleNum, sample_cty, sample_st, q_25, q_75, aggBias, match

    stopifnot(length(modCodeLs) == 2L) # must have two model codes
    stopifnot(("8f" %in% unlist(strsplit(modCodeLs, "_"))) & ("10f" %in% unlist(strsplit(modCodeLs, "_")))) # must have county and state model codes

    # import and merge posterior samples from state and county models
    importDat1 <- import_fit_posteriorSamples(modCodeLs[1])
    importDat2 <- import_fit_posteriorSamples(modCodeLs[2])
    
    importDat <- full_join(importDat1, importDat2, by = c("season", "fips_st", "sampleNum")) %>%
        mutate(aggBias = sample_st - sample_cty) 

    summStats_aggBias <- importDat %>%
        group_by(season, fips_st) %>%
        summarise(q_25 = quantile(aggBias, .25), q_75 = quantile(aggBias, .75)) 

    # define match as aggBias falling within IQR (smallest 50% magnitude in error)
    fullDat <- full_join(importDat, summStats_aggBias, by = c("season", "fips")) %>% 
        mutate(match = ifelse(aggBias > q_25 & aggBias < q_75, TRUE, FALSE))

    return(fullDat)
}

#### import predictor data ################################
################################
import_predictorData_2x2 <- function(filepathList){
    print(match.call())
    # import model predictors and prepare for counting in 2x2 contingency table for OR calculation
    # predictorDat cols: season, fips, fips_st, predictor, value, hivalue

    modDat <- model8f_wksToEpi_v7(filepathList)
    predictorDat <- modDat %>%
        select(season, fips, fips_st, contains("X_"), contains("O_")) %>%
        select(-contains("graphIdx")) %>%
        gather(predictor, value, contains("X_"), contains("O_")) %>%
        filter(!is.na(value)) %>%
        mutate(hivalue = ifelse(value > 0, TRUE, FALSE))

    return(predictorDat)
}
################################
import_predictorDiscrepancy_2x2 <- function(filepathList){
    print(match.call())
    # import model predictors and prepare for counting in 2x2 contingency table for OR calculation
    # fullDat cols: season, fips, fips_st, predictor, value_cty, value_st, discrepMag

    ctyDat <- model8f_wksToEpi_v7(filepathList) %>%
        select(season, fips, fips_st, contains("X_"), contains("O_")) %>%
        select(-contains("graphIdx")) %>%
        gather(predictor, value_cty, contains("X_"), contains("O_"))
    stDat <- model10f_wksToEpi_v2(filepathList) %>%
        select(season, fips_st, contains("X_"), contains("O_")) %>%
        gather(predictor, value_st, contains("X_"), contains("O_"))
    fullDat <- full_join(ctyDat, stDat, by = c("season", "fips_st", "predictor")) %>%
        mutate(discrep = value_st - value_cty) %>%
        mutate(discrepMag = abs(value_st - value_cty)) %>%
        filter(!is.na(discrepMag)) 

    return(fullDat)
}

#### merge bias and predictor data ################################
################################
merge_aggBias_predictor_data <- function(matchDat, filepathList){
    print(match.call())
    # merge dataframes with aggBias and raw predictor data 

    predDat <- import_predictorData_2x2(filepathList) %>%
        select(-hivalue) %>%
        spread(predictor, value)
        
    fullDat <- full_join(matchDat, predDat, by = c("season", "fips"))
    return(fullDat)
}
################################
merge_aggBias_predictorDiscrep_data <- function(matchDat, filepathList){
    print(match.call())
    # merge dataframes with aggBias and raw predictor data 

    predDiscrepDat <- import_predictorDiscrepancy_2x2(filepathList) %>%
        select(-value_cty, -value_st, -discrepMag) %>%
        spread(predictor, discrep)
    
    fullDat <- full_join(matchDat, predDiscrepDat, by = c("season", "fips"))
    return(fullDat)
}
################################
merge_aggBias_predictorDiscrepMag_data <- function(matchDat, filepathList){
    print(match.call())
    # merge dataframes with aggBias and raw predictor data 

    predDiscrepMagDat <- import_predictorDiscrepancy_2x2(filepathList) %>%
        select(-value_cty, -value_st, -discrep) %>%
        spread(predictor, discrepMag)
    
    fullDat <- full_join(matchDat, predDiscrepMagDat, by = c("season", "fips"))
    return(fullDat)
}

#### merge bias and predictor data ################################
pairs_scatterplotMatrix_aggBias <- function(full_df){
  # return scatterplot matrix of all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>%
    select(bias, biasMag, contains("X_"), contains("O_")) 
  
  pairPlt <- ggpairs(datOnly) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  return(pairPlt)
}
################################
pairs_corrMatrix_aggBias <- function(full_df){
  # return correlation matrix for all variables pooled across states & seasons
  print(match.call())
    
  datOnly <- full_df %>%
    mutate(fipsSeas = paste(fips, season, sep="-")) %>%
    select(bias, biasMag, contains("X_"), contains("O_")) 

  return(ggcorr(datOnly, method = c("pairwise", "spearman"), label = TRUE, high = "#3B9AB2", low = "#F21A00", legend.position = "bottom", layout.exp = 1, hjust = 0.85))
}

