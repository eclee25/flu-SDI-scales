# ## Name: Elizabeth Lee
# ## Date: 11/4/17
# ## Function: main code to correct for aggregation bias, aggregation bias response variable models were applied with single variables
# ## Filenames: 
# ## Data Source: 
# ## Notes: 
# ## 
# ## useful commands:
# ## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
# ## update.packages(lib.loc = "/usr/local/lib/R/site-library")
# rm(list = ls())
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
source("source_correct_aggBias_rd2_functions.R")
# 
# #### set these ################################
# 
# dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
# measure <- "wksToEpi"
# coefName <- "X_anomHumidity"
# modCodeStr <- "11f_wksToEpi_anomHumidity"
# seed <- 944
# set.seed(seed)
# 
# #### PATHS ##################################
# setwd('../reference_data')
# path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
# path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
# path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
# path_latlon_reg <- paste0(getwd(), "/region_latlon.csv")
# path_region_cw <- paste0(getwd(), "/state_abbreviations_FIPS_region.csv")
# 
# setwd("../R_export")
# path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
# path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
# path_response_reg <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))
# path_list <- list(path_abbr_st = path_abbr_st, 
#                   path_region_cw = path_region_cw,
#                   path_latlon_st = path_latlon_st,
#                   path_latlon_cty = path_latlon_cty,
#                   path_latlon_reg = path_latlon_reg,
#                   path_response_st = path_response_st,
#                   path_response_reg = path_response_reg,
#                   path_response_cty = path_response_cty)
# 
# ################################
# #### MAIN ####
# setwd(dirname(sys.frame(1)$ofile))
# 
# #### IMPORT BURDEN DATA ####
# if(measure == "wksToEpi"){
#   # trueCtyObsDat <- import_obs_wksToEpi(path_list)
#   trueStObsDat <- import_obs_wksToEpi_st(path_list)
# } else if(measure == "wksToPeak"){
#   trueCtyObsDat <- import_obs_wksToPeak(path_list)
#   trueStObsDat <- import_obs_wksToPeak_st(path_list)
# } else if(measure == "iliEarly"){
#   trueCtyObsDat <- import_obs_iliEarly(path_list)
#   trueStObsDat <- import_obs_iliEarly_st(path_list)
# } else if(measure == "iliPeak"){
#   trueCtyObsDat <- import_obs_iliPeak(path_list)
#   trueStObsDat <- import_obs_iliPeak_st(path_list)
# } 
# 
# #### IMPORT INPUT DATA ####
# postSamplesDat <- import_posteriorSamples_data(modCodeStr)

coefName <- "X_rnorm"
predDat <- grab_predictor_data(modCodeStr, coefName)


#### IMPORT PREDICTOR DATA ####
ctyStar <- calculate_new_cty_burden(postSamplesDat, trueStObsDat, predDat)

#### PLOT PREDICTOR DATA ####
scatFormats <- list(w = 6, h = 4, measure = measure, coefName = coefName)
scatterPlt <- scatter_cty_ctyStar(ctyStar, scatFormats)
