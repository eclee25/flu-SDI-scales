
## Name: Elizabeth Lee
## Date: 10/31/17
## Function: main code to generate MS figures
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_msFigs.R")
source("source_import_modeldata.R")

################################
dbCodeStr <- "_irDt_Octfit_span0.4_degree2"

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
#### MAIN ####
setwd(dirname(sys.frame(1)$ofile))

#### IMPORT DATA ####
datFormats_st <- list(offset_l = FALSE, bigscale = "st")
datFormats_reg <- list(offset_l = FALSE, bigscale = "reg")
datFormats_correlog <- list(dataScale = "cty", resamp = 500)


# obsDat <- import_obs_allMeasures_cty(path_list)
# aggBiasDat_st <- import_obs_aggBias_allMeasures(path_list, datFormats_st)
# aggBiasDat_reg <- import_obs_aggBias_allMeasures(path_list, datFormats_reg)
# correlogDat <- import_obs_correlog(datFormats_correlog) %>%
#   left_join(measure_labels(), by = "measure")

# #### FIGURE 1 ########################################################
# #### ALLMEASURES - CHOROS - ONE SEASON ####
# choro_obs_formats <- list(w = 3, h = 1.8, legendStep = 4)
# choro_obs_timingMeasures_oneSeason(obsDat, choro_obs_formats)
# choro_obs_formats <- list(w = 3, h = 1.8, legendStep = 0.5)
# choro_obs_magnitudeMeasures_oneSeason(obsDat, choro_obs_formats)

# #### ALLMEASURES - AggBias - CHOROS - ONE SEASON ####
# choro_aggBias_formats <- list(w = 6, h = 1.8)
# choro_obs_aggBias_allMeasures_oneSeason(aggBiasDat_st, choro_aggBias_formats)


# #### FIGURE  ########################################################
# #### ALLMEASURES - CORRELOGRAMS - ONE SEASON ####
# correlog_obs_formats <- list(w = 6, h = 3, dataScale = "cty")
# correlog_obs_allMeasures(correlogDat, correlog_obs_formats)

#### FIGURE  ########################################################
#### ROC CURVES ####
# no data import dependence
line_corrFactor_formats <- list(w = 6, h = 3)
line_corrFactor_auc(line_corrFactor_formats)