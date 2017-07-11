## Name: Elizabeth Lee
## Date: 7/10/17
## Function: main code to analyze aggregation bias
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_aggBias_explore_functions.R")

###############################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

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

# choropleth of overlap between state and county fitted weeks to epi onset
plotFormats <- list(w = 3, h = 2.5)
dataFormats <- list(offset_l = FALSE)
choro_fitOverlap_stCty_wksToEpi_oneSeason("8f_wksToEpi_v2-4", "10f_wksToEpi_v1-3", plotFormats, dataFormats, path_list)

# scatterplot of state versus county
plotFormats <- list(w = 6, h = 4)
dataFormats <- list(offset_l = FALSE)
scatter_fitCompare_stCty_wksToEpi("8f_wksToEpi_v2-4", "10f_wksToEpi_v1-3", plotFormats, dataFormats, path_list)



