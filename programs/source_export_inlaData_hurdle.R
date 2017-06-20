
## Name: Elizabeth Lee
## Date: 7/21/16
## Function: general functions to export hurdle model INLA results as data files -- county
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2)
source("source_export_inlaDiagnostics.R")


#### functions for data export  ################################
################################

export_summaryStats_hurdle_likString <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season, likString){
  # 12/15/16 export summary statistics of INLA model output for hurdle model variables (flex non-zero likelihood string) -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.hyperpar) <- names(modelOutput$summary.fixed)[1:6] # 8/17/16 add hyperpar export
  
  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), likString, NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  # clean hyperpar summary statistics output from INLA
  summaryHyperpar <- tbl_df(modelOutput$summary.hyperpar) %>%
    mutate(RV = rownames(modelOutput$summary.hyperpar)) %>%
    mutate(effectType = "hyperpar", kld = NA) %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), likString, NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  summaryComplete <- bind_rows(summaryFixed, summaryHyperpar)
  # clean random effects summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$fips_nonzero)){
    names(modelOutput$summary.random$fips_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomFips <- modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = likString) %>%
      mutate(effectType = "spatial") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomFips)
  }
  # clean structure spatial effects  summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$graphIdx_nonzero)){
    names(modelOutput$summary.random$graphIdx_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomGraphid <- modelOutput$summary.random$graphIdx_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("phi", RV))) %>%
      mutate(effectType = "structured") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomGraphid)
  }
  if (!is.null(modelOutput$summary.random$graphIdx_st_nonzero)){
    names(modelOutput$summary.random$graphIdx_st_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomGraphid2 <- modelOutput$summary.random$graphIdx_st_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("phi", RV))) %>%
      mutate(effectType = "structured_st") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomGraphid2)
  }
  if (!is.null(modelOutput$summary.random$fips_st_nonzero)){
    names(modelOutput$summary.random$fips_st_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSt <- modelOutput$summary.random$fips_st_nonzero %>% mutate(likelihood = likString) %>%
      mutate(effectType = "stID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSt)
  }
  if (!is.null(modelOutput$summary.random$regionID_nonzero)){
    names(modelOutput$summary.random$regionID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomReg <- modelOutput$summary.random$regionID_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("R", RV))) %>% ## 10/26/16: paste "R"
      mutate(effectType = "regID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomReg)
  }
  if (!is.null(modelOutput$summary.random$season_nonzero)){
    names(modelOutput$summary.random$season_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSeas <- modelOutput$summary.random$season_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = paste0("S", RV)) %>%
      mutate(effectType = "season") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSeas)
  }
  # 10/26/16: added error term for each observation
  if (!is.null(modelOutput$summary.random$ID_nonzero)){
    names(modelOutput$summary.random$ID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomErr <- modelOutput$summary.random$ID_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(RV)) %>%
      mutate(effectType = "error") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomErr)
  }
  
  # bind data together
  summaryStats <- summaryComplete %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats_fitted_aggBias <- function(exportPath, oneLik_fits, modDataFullOutput, modCodeString, dbCodeString, season){
  # process aggbias model fitted values for diagnostic plotting
  print(match.call())
  
  names(oneLik_fits) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode")
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips, ID, fit_rr_st, fit_rr_cty, y, season), oneLik_fits) %>% 
      mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, exportDate = as.character(Sys.Date())) %>% # 10/11/16: grab season from modDataFullOutput instead of function argument
      select(modCodeStr, dbCodeStr, season, exportDate, fips, ID, mean, sd, q_025, q_5, q_975, mode, fit_rr_st, fit_rr_cty, y)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
################################

