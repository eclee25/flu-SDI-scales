## Name: Elizabeth Lee
## Date: 7/10/17
## Function: main code to analyze aggregation bias from model outcomes for wksToEpi
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_aggBias_data_explore_functions.R")

#### set these! ###############################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr_cty <- "8f_wksToEpi_v2-8"
modCodeStr_st <- "10f_wksToEpi_v1-3"
modCodeStr_reg <- ""

###############################
## PATHS ##
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
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

#### weeks to epi onset ############################
# scatterplot of state versus county/region
plotFormats <- list(w = 6, h = 4)
dataFormats <- list(offset_l = FALSE)
scatter_obsCompare_stCty_wksToEpi(modCodeStr_cty, modCodeStr_st, plotFormats, dataFormats, path_list)
scatter_obsCompare_regCty_wksToEpi(modCodeStr_cty, plotFormats, dataFormats, path_list) 
scatter_obsCompare_stCty_wksToPeak(plotFormats, dataFormats, path_list)
scatter_obsCompare_regCty_wksToPeak(plotFormats, dataFormats, path_list) 
scatter_obsCompare_stCty_iliEarly(plotFormats, dataFormats, path_list)
scatter_obsCompare_regCty_iliEarly(plotFormats, dataFormats, path_list) 
scatter_obsCompare_stCty_iliPeak(plotFormats, dataFormats, path_list)
scatter_obsCompare_regCty_iliPeak(plotFormats, dataFormats, path_list) 


# choropleth of magnitude of aggregation bias
plotFormats <- list(w = 6, h = 4)
dataFormats <- list(offset_l = FALSE)
choro_obs_aggBias_stCty_wksToEpi_oneSeason(modCodeStr_cty, modCodeStr_st, plotFormats, dataFormats, path_list)
choro_obs_aggBias_regCty_wksToEpi_oneSeason(modCodeStr_cty, plotFormats, dataFormats, path_list)
choro_obs_aggBias_stCty_wksToPeak_oneSeason(plotFormats, dataFormats, path_list)
choro_obs_aggBias_regCty_wksToPeak_oneSeason(plotFormats, dataFormats, path_list)
choro_obs_aggBias_stCty_iliEarly_oneSeason(plotFormats, dataFormats, path_list)
choro_obs_aggBias_regCty_iliEarly_oneSeason(plotFormats, dataFormats, path_list)
choro_obs_aggBias_stCty_iliPeak_oneSeason(plotFormats, dataFormats, path_list)
choro_obs_aggBias_regCty_iliPeak_oneSeason(plotFormats, dataFormats, path_list)

# Interpretation: positive error (green) means that state model predicted a later epidemic onset than the county model 

