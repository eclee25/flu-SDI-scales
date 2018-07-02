# ## Name: Elizabeth Lee
# ## Date: 5/28/18
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
source("source_import_modeldata_aggBias_rd3.R")
source("source_correct_aggBias_rd3_functions.R")

#### set these ################################

dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
modCodeStr <- "10h_iliEarly_v1-1" # 10a_iliSum_v1-1, 10b_iliPeak_v1-1, 10h_iliEarly_v1-1, 10f_wksToEpi_v1-6
measure <- unlist(strsplit(modCodeStr, "_"))[2]
seed <- 944
set.seed(seed)

#### PATHS ##################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
path_latlon_reg <- paste0(getwd(), "/region_latlon.csv")
path_region_cw <- paste0(getwd(), "/state_abbreviations_FIPS_region.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_response_reg <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st, 
                  path_region_cw = path_region_cw,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_latlon_reg = path_latlon_reg,
                  path_response_st = path_response_st,
                  path_response_reg = path_response_reg,
                  path_response_cty = path_response_cty)

################################
#### MAIN ####
setwd(dirname(sys.frame(1)$ofile))

#### IMPORT BURDEN DATA ####
## current method does not work with timing measures
if(measure == "wksToEpi"){
  trueCtyObsDat <- import_obs_wksToEpi(path_list)
  trueStObsDat <- import_obs_wksToEpi_st(path_list)
  calculate_new_cty_burden <- calculate_new_cty_burden_timing
} else if(measure == "wksToPeak"){
  trueCtyObsDat <- import_obs_wksToPeak(path_list)
  trueStObsDat <- import_obs_wksToPeak_st(path_list)
  calculate_new_cty_burden <- calculate_new_cty_burden_timing
} else if(measure == "iliEarly"){
  trueCtyObsDat <- import_obs_iliEarly(path_list)
  trueStObsDat <- import_obs_iliEarly_st(path_list)
  calculate_new_cty_burden <- calculate_new_cty_burden_mag
} else if(measure == "iliPeak"){
  trueCtyObsDat <- import_obs_iliPeak(path_list)
  trueStObsDat <- import_obs_iliPeak_st(path_list)
  calculate_new_cty_burden <- calculate_new_cty_burden_mag
} else if(measure == "iliSum"){
  trueCtyObsDat <- import_obs_iliSum(path_list)
  trueStObsDat <- import_obs_iliSum_st(path_list)
  calculate_new_cty_burden <- calculate_new_cty_burden_mag
} 

## 5/29/18 checked that sum of county populations is equivalent to state population

#### IMPORT INPUT DATA ####
postSamplesDat <- import_posteriorSamples_data_st(modCodeStr)

#### IMPORT PREDICTOR DATA ####
ctyStarDat <- calculate_new_cty_burden(postSamplesDat, trueCtyObsDat)
compareDat <- compare_true_star_ctyDat(trueCtyObsDat, ctyStarDat)
# WRITE FXN FOR RANK ORDER COMPARISONS
## explore matches and residuals
p1 <- ggplot(compareDat %>% dplyr::filter(!is.na(match)), aes(x = true_y, y = q_5)) +
  geom_point() +
  geom_errorbar(aes(ymin = q_025, ymax = q_975)) +
  theme_bw() +
  facet_wrap(~season, scales = "free_y")
p2 <- ggplot(compareDat %>% dplyr::filter(!is.na(match)), aes(x = q_5, y = std_resid)) + 
  geom_point(aes(colour = match)) + 
  theme_bw() +
  facet_wrap(~season, scales = "free_y")
hist(compareDat$std_resid)

print(p1)
print(p2)

# p2 <- ggplot(compareDat, aes(x = )) + geom_histogram() + facet_wrap(~season)

# # #### PLOT PREDICTOR DATA ####
# # scatFormats <- list(w = 6, h = 4, measure = measure, coefName = coefName)
# scatterPlt <- scatter_cty_ctyStar(ctyStar, scatFormats)
