## Name: Elizabeth Lee
## Date: 6/23/17
## Function: main code to analyze subsample bias (region models compared to county surveillance model)
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_subsampleBias_explore_functions.R")
source("source_aggBias_explore_functions.R")

################################
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

plotFormats <- list(w = 3, h = 2.5)

# subsample bias
modCodeSsList <- c("8a_iliSum_v2-6_R1&2&3", "8a_iliSum_v2-6_R4&6", "8a_iliSum_v2-6_R5&7", "8a_iliSum_v2-6_R8&9&10")
lapply(modCodeSsList,
       function(x, ctyCode, pltFormats, paths) choro_fit_subsampleBias_seasIntensityRR_oneSeason(ctyCode, x, pltFormats, paths),
       ctyCode = "8a_iliSum_v2-6",
       pltFormats = plotFormats,
       paths = path_list)

# aggregation bias
choro_fit_aggBias_seasIntensityRR_oneSeason("8a_iliSum_v2-6", "10a_iliSum_v1-2", plotFormats, path_list)

