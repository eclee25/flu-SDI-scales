## Name: Elizabeth Lee
## Date: 11/4/17
## Function: main code to correct for aggregation bias
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
source("source_correct_aggBias_functions.R")

#### set these ################################

dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
measure <- "wksToEpi"
coefName <- "X_anomHumidity"
modCodeStr <- "8f_wksToEpi_v2-21"

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
if(measure == "wksToEpi"){
  trueCtyObsDat <- import_obs_wksToEpi(path_list)
  trueStObsDat <- import_obs_wksToEpi_st(path_list)
} else if(measure == "wksToPeak"){
  trueCtyObsDat <- import_obs_wksToPeak(path_list)
  trueStObsDat <- import_obs_wksToPeak_st(path_list)
} else if(measure == "iliEarly"){
  trueCtyObsDat <- import_obs_iliEarly(path_list)
  trueStObsDat <- import_obs_iliEarly_st(path_list)
} else if(measure == "iliPeak"){
  trueCtyObsDat <- import_obs_iliPeak(path_list)
  trueStObsDat <- import_obs_iliPeak_st(path_list)
} 

#### APPLY CLASSIFICATIONS TO CTY BURDEN ####
trueClassif_cty <- apply_true_classifications_cty(trueCtyObsDat)
trueClassif_st <- apply_true_classifications_st(trueCtyObsDat)

#### CREATE CLASSIFIER DATA ####
proxyDat <- create_proxy_data(modCodeStr, coefName, trueStObsDat)
testClassif_cty <- create_classifier_data_cty(proxyDat)
testClassif_st <- create_classifier_data_st(proxyDat)

#### MERGE DATA ####
fullClassif_cty <- full_join(trueClassif_cty, testClassif_cty, by = c("season", "fips", "quantLvl"))
fullClassif_st <- full_join(trueClassif_st, testClassif_st, by = c("season", "fips", "quantLvl"))

ctyROC <- roc(as.numeric(trueClassif) ~ as.numeric(testClassif), data = fullClassif_cty, 
  auc = TRUE, na.rm = TRUE, plot = TRUE)
stROC <- roc(as.numeric(trueClassif) ~ as.numeric(testClassif), data = fullClassif_st, auc = TRUE, na.rm = TRUE, plot = TRUE)

#### CALCULATE SENSITIVITY AND SPECIFICITY, PREPARE PLOT DATA ####
plotDat_cty <- identify_sensitive_specific(fullClassif_cty)
plotDat_st <- identify_sensitive_specific(fullClassif_st)

#### PLOT DATA ####
fname <- paste0(getwd(), "/../graph_outputs/correct_aggBias/rocCty_", gsub("X_", "", coefName), "_", modCodeStr, ".png")

png(filename = fname, units = "in", width = 4, height = 4, res = 300)
plot(plotDat_cty$fpr, plotDat_cty$sensitivity, xlab = "False Positive Rate (1-Specificity)", ylab = "Sensitivity", main = "") 
dev.off()
# plot(plotDat_st$fpr, plotDat_st$sensitivity, xlab = "False Positive Rate", ylab = "Sensitivity")
