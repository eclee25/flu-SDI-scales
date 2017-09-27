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
modules <- c("choroAvg") # "statistics", "scatterplot", "choro", "choroAvg"

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

#### import aggBias data ############################
offsetSetting <- FALSE
dataParams <- list(offset_l = offsetSetting, filepathList = path_list)

obs_wksToEpi_ctySt <- do.call(import_obs_wksToEpi_ctySt, c(dataParams))
obs_wksToEpi_ctyReg <- do.call(import_obs_wksToEpi_ctyReg, c(dataParams))
obs_wksToPeak_ctySt <- do.call(import_obs_wksToPeak_ctySt, c(dataParams))
obs_wksToPeak_ctyReg <- do.call(import_obs_wksToPeak_ctyReg, c(dataParams))

obs_iliEarly_ctySt <- do.call(import_obs_iliEarly_ctySt, c(dataParams))
obs_iliEarly_ctyReg <- do.call(import_obs_iliEarly_ctyReg, c(dataParams))
obs_iliPeak_ctySt <- do.call(import_obs_iliPeak_ctySt, c(dataParams))
obs_iliPeak_ctyReg <- do.call(import_obs_iliPeak_ctyReg, c(dataParams))

#### statistics ############################
if("statistics" %in% modules){
  timeSt <- pairedTest_aggBias_timingMagnitude(obs_wksToEpi_ctySt, obs_wksToPeak_ctySt)
  timeReg <- pairedTest_aggBias_timingMagnitude(obs_wksToEpi_ctyReg, obs_wksToPeak_ctyReg)
  magSt <- pairedTest_aggBias_timingMagnitude(obs_iliEarly_ctySt, obs_iliPeak_ctySt)
  magReg <- pairedTest_aggBias_timingMagnitude(obs_iliEarly_ctyReg, obs_iliPeak_ctyReg)
  # list(histPlot=histPlot, absHistPlot=absHistPlot, dbPlot=dbPlot, absDbPlot=absDbPlot, ttest=ttest, absTtest=absTtest)
}

#### plots ############################
# scatterplot of state versus county/region
if("scatterplot" %in% modules){
  staticFormats <- list(w = 6, h = 4, offset_l = FALSE)
  dynFormatLs <- data.frame(measure = c(rep("wksToEpi", 2), rep("wksToPeak", 2), rep("iliEarly", 2), rep("iliPeak", 2)), bigscale = rep(c("st", "reg"), 4))
  dynFormats <- split(dynFormatLs, seq(nrow(dynFormatLs)))

  scatter_obsCompare_aggBias_timingMagnitude(obs_wksToEpi_ctySt, staticFormats, dynFormats[[1]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_wksToEpi_ctyReg, staticFormats, dynFormats[[2]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_wksToPeak_ctySt, staticFormats, dynFormats[[3]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_wksToPeak_ctyReg, staticFormats, dynFormats[[4]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_iliEarly_ctySt, staticFormats, dynFormats[[5]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_iliEarly_ctyReg, staticFormats, dynFormats[[6]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_iliPeak_ctySt, staticFormats, dynFormats[[7]])
  scatter_obsCompare_aggBias_timingMagnitude(obs_iliPeak_ctyReg, staticFormats, dynFormats[[8]])
}

############################
# choropleth of magnitude of aggregation bias - one season
if("choro" %in% modules){
  plotFormats <- list(w = 6, h = 4)
  choro_obs_aggBias_stCty_wksToEpi_oneSeason(obs_wksToEpi_ctySt, plotFormats)
  choro_obs_aggBias_regCty_wksToEpi_oneSeason(obs_wksToEpi_ctyReg, plotFormats)
  choro_obs_aggBias_stCty_wksToPeak_oneSeason(obs_wksToPeak_ctySt, plotFormats)
  choro_obs_aggBias_regCty_wksToPeak_oneSeason(obs_wksToPeak_ctyReg, plotFormats)
  choro_obs_aggBias_stCty_iliEarly_oneSeason(obs_iliEarly_ctySt, plotFormats)
  choro_obs_aggBias_regCty_iliEarly_oneSeason(obs_iliEarly_ctyReg, plotFormats)
  choro_obs_aggBias_stCty_iliPeak_oneSeason(obs_iliPeak_ctySt, plotFormats)
  choro_obs_aggBias_regCty_iliPeak_oneSeason(obs_iliPeak_ctyReg, plotFormats)

  # Interpretation: positive error (green) means that state model predicted a later epidemic onset than the county model
}
 
############################
# choropleth of magnitude of aggregation bias - average seasons
if("choroAvg" %in% modules){
  plotFormatsDf <- tbl_df(data.frame(
    dbCode = c(rep("wksToEpi", 2), rep("wksToPeak", 2), rep("iliEarly", 2), rep("iliPeak", 2)), 
    scaleDiff = rep(c("stCty", "regCty"), 4), 
    # breaks = ,
    w = 6, 
    h = 4)) %>%
    mutate(pltVar = paste0("obs_diff_", scaleDiff))

  breaksDf <- data.frame(
    wksToEpi_ctySt = c(-14, -5, -3, -1, 1, 3, 5, 8), 
    wksToEpi_ctyReg = c(-15, -6, -3,-1, 1, 3, 6, 9),
    wksToPeak_ctySt = c(-10, -4, -2, -1, 1, 2, 4, 7),
    wksToPeak_ctyReg = c(-8, -3, -2, -1, 1, 2, 3, 7),
    iliEarly_ctySt = c(-20, -4, -2, -1, 1, 2, 4, 5),
    iliEarly_ctyReg = c(-21, -4, -2, -1, 1, 2, 4, 6),
    iliPeak_ctySt = c(-31, -4, -2, -1, 1, 2, 4, 6),
    iliPeak_ctyReg = c(-34, -6, -3, -1, 1, 2, 3.5, 5))

  choro_obs_aggBias_avgSeason(obs_wksToEpi_ctySt, as.list(plotFormatsDf[1,]), breaksDf$wksToEpi_ctySt)
  choro_obs_aggBias_avgSeason(obs_wksToEpi_ctyReg, as.list(plotFormatsDf[2,]), breaksDf$wksToEpi_ctyReg)
  choro_obs_aggBias_avgSeason(obs_wksToPeak_ctySt, as.list(plotFormatsDf[3,]), breaksDf$wksToPeak_ctySt)
  choro_obs_aggBias_avgSeason(obs_wksToPeak_ctyReg, as.list(plotFormatsDf[4,]), breaksDf$wksToPeak_ctyReg)
  choro_obs_aggBias_avgSeason(obs_iliEarly_ctySt, as.list(plotFormatsDf[5,]), breaksDf$iliEarly_ctySt)
  choro_obs_aggBias_avgSeason(obs_iliEarly_ctyReg, as.list(plotFormatsDf[6,]), breaksDf$iliEarly_ctyReg)
  choro_obs_aggBias_avgSeason(obs_iliPeak_ctySt, as.list(plotFormatsDf[7,]), breaksDf$iliPeak_ctySt)
  choro_obs_aggBias_avgSeason(obs_iliPeak_ctyReg, as.list(plotFormatsDf[8,]), breaksDf$iliPeak_ctyReg)
  # ADD FUNCTION TO WRITE AGGBIAS FROM THE DATA CALCULATED IN THESE FUNCTIONS

  # Interpretation: positive error (green) means that state model predicted a later epidemic onset than the county model
}

