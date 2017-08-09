## Name: Elizabeth Lee
## Date: 7/14/17
## Function: main code to compare wksToEpi and wksToPeak measures
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_wksToEpiAndPeak_explore_functions.R")

#### set these! ###############################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr_epi <- "8f_wksToEpi_v2-4"
modCodeStr_peak <- ""

###############################
## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_st = path_response_st,
                  path_response_cty = path_response_cty,
                  path_fullIndic_cty = path_fullIndic_cty)


################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

#### observed  ############################

#### scatter of observed county/state comparisons ########################
# plotFormats <- list(w = 6, h = 4)
# dataFormats <- list(offset_l = FALSE)

# # scatterplot of observed county comparisons
# scatter_obsCompare_wksToEpiAndPeak_cty(path_list, plotFormats, dataFormats)
# # scatterplot of observed state comparisons
# scatter_obsCompare_wksToEpiAndPeak_st(path_list, plotFormats, dataFormats)


#### explore late onset time series ########################
plotFormats <- list(w = 6, h = 4, sampleSize = 36, numPerPlot = 6, seed = 23150)
tsExplore_obs_wksToEpi_cty(path_list, plotFormats)

# 7/14/17 checks 
# Issue: troubling negative relationship between wksToEpi and wksToPeak because wksToPeak can endure only Nov through Mar and wksToEpi can only occur from Oct through Mar
# 1) Are late-starting seasons getting truncated such that the peak is simply the last week in the epi period?
# 2) Conversely, are early starting seasons post-peak (or at peak) at the beginning of the epi period?
# Checks:
#  - sample random fips-season combos where weeksToEpi > 20 and plot time series and inSeason
#  - sample random fips-season combos where weeksToPeak < 7 and plot time series and inSeason
# Maybe epi threshold needs to be more strict
# Alternative measure that may be less sensitive to data processing would be wksToPeak where origin is week 40


#### fitted ############################



