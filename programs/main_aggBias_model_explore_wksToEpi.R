## Name: Elizabeth Lee
## Date: 7/10/17
## Function: main code to analyze aggregation bias from model outcomes for wksToEpi
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_aggBias_model_explore_functions.R")

#### set these! ###############################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr_cty <- "8f_wksToEpi_v2-4"
modCodeStr_st <- "10f_wksToEpi_v1-3"
modCodeStr_reg <- ""

###############################
## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_st = path_response_st,
                  path_response_cty = path_response_cty)

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

#### weeks to epi onset ############################
# choropleth of overlap between state and county fitted weeks to epi onset
plotFormats <- list(w = 3, h = 2.5)
dataFormats <- list(offset_l = FALSE)
choro_fitOverlap_stCty_wksToEpi_oneSeason(modCodeStr_cty, modCodeStr_st, plotFormats, dataFormats, path_list)
choro_fitOverlap_regCty_wksToEpi_oneSeason(modCodeStr_cty, modCodeStr_reg, plotFormats, dataFormats, path_list) # 9/15/17 need to write function in source_aggBias_model_explore_functions.R, run region models?

# scatterplot of state versus county
plotFormats <- list(w = 6, h = 4)
dataFormats <- list(offset_l = FALSE)
scatter_fitCompare_stCty_wksToEpi(modCodeStr_cty, modCodeStr_st, plotFormats, dataFormats, path_list)
scatter_fitCompare_regCty_wksToEpi(modCodeStr_cty, modCodeStr_reg, plotFormats, dataFormats, path_list) # 9/15/17 need to write function in source_aggBias_model_explore_functions.R, run region models?

plotFormats <- list(w = 3, h = 2.5)
dataFormats <- list(offset_l = FALSE)
# choropleth of magnitude of aggregation bias
## CHECK BREAKS FOR AGGBIAS BEFORE RUNNING ##
choro_fit_aggBias_stCty_wksToEpi_oneSeason(modCodeStr_cty, modCodeStr_st, plotFormats, dataFormats, path_list)
choro_fit_aggBias_regCty_wksToEpi_oneSeason(modCodeStr_cty, modCodeStr_reg, plotFormats, dataFormats, path_list) # 9/15/17 need to write function in source_aggBias_model_explore_functions.R, run region models?

# Interpretation: positive error (green) means that state model predicted a later epidemic onset than the county model 

