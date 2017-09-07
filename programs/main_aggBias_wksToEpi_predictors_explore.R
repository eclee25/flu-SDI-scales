
## Name: Elizabeth Lee
## Date: 8/31/2017
## Function: EDA suite of variable selection analyses for wksToEpi aggregation bias at county level
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB.csv, source_clean_data_function.R
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# rm(list = ls())
require(tidyverse) 
require(RColorBrewer)

#### set these! ################################
modCode_cty <- "8f_wksToEpi_v2-8"
modCode_st <- "10f_wksToEpi_v1-6"
agecode <- ""
dbCodeStr <- sprintf("_ilinDt_Octfit%s_span0.4_degree2", agecode)
rCode <- "wksToEpi " 
seasons <- 3:9
analysesOn <- c('pairwise') 
# 'loadData', 'dataQuality', 'pairwise', 'singleVarWrite', 'singleVarPlot'
origin_locations_file <- "Lee"

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_aggBias_predictors_explore_functions.R")

#### FILEPATHS #################################
setwd('../reference_data')
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_fullIndic_st <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
fname_coefDat <- sprintf("/VS_coefDat_%s_cty", rCode)
path_coefDat <- paste0(getwd(), fname_coefDat)
path_tempDatQuality <- paste0(getwd(), sprintf("/VS_tempDatQuality_%s_cty.csv", rCode))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

setwd("./origin_locations")
path_srcLoc_cty <- paste0(getwd(), sprintf("/fluseason_source_locations_%s.csv", origin_locations_file))
path_srcLoc_st <- paste0(getwd(), sprintf("/fluseason_source_locations_%s.csv", origin_locations_file))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_latlon_st = path_latlon_st,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty,
                  path_response_st = path_response_st,
                  path_graphIdx_cty = path_graphIdx_cty,
                  path_graphExport_st = path_graphExport_st,
                  path_graphIdx_st = path_graphIdx_st,
                  path_fullIndic_cty = path_fullIndic_cty,
                  path_fullIndic_st = path_fullIndic_st,
                  path_srcLoc_cty = path_srcLoc_cty,
                  path_srcLoc_st = path_srcLoc_st)

setwd(dirname(sys.frame(1)$ofile))
setwd("../graph_outputs")
path_pltExport <- paste0(getwd(), "/aggBias_wksToEpi_predictors_explore")

#### PLOT FORMATTING ################################
w <- 12; h <- 10; dp <- 300
w2 <- 6; h2 <- 3
setwd(path_pltExport)

#### MAIN #################################################################

#### Import and process response & covariate data ####################################
if("loadData" %in% analysesOn){
  
  # load aggBias and aggBiasMagnitude data
  matchDat <- import_fit_aggBias_wksToEpi(modCode_cty, modCode_st, path_list) %>%
        select(season, fips, bias, biasMag)

  # merge aggBias data frame with different versions of predictor data
  predDat <- merge_aggBias_predictor_data(matchDat, path_list) # aggBias, predictor
  discrepDat <- merge_aggBias_predictorDiscrep_data(matchDat, path_list)
  discrepMagDat <- 
    merge_aggBias_predictorDiscrepMag_data(matchDat, path_list)
  
} # end loadData

#### Check data quality ####################################
if("dataQuality" %in% analysesOn){
  
  # output county counts by season for each variable: VS_tempDatQuality_%s_cty.csv
  # check counts in each column
  dataQuality <- allDat %>% group_by(season) %>% summarise_each(funs(ct = sum(!is.na(.))))
  write_csv(dataQuality, path_tempDatQuality)
  
} # end dataQuality

#### Pairwise variable comparisons ####################################
if("pairwise" %in% analysesOn){
  
  ## predictor data ##
  png(sprintf("scatterMx_pred_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  scatterMx <- pairs_scatterplotMatrix_aggBias(predDat)
  print(scatterMx)
  dev.off()

  png(sprintf("corrMx_spearman_pred_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  corrMx <- pairs_corrMatrix_aggBias(predDat)
  print(corrMx)
  dev.off()

  ## st-cty discrepancy data ##
  png(sprintf("scatterMx_discrep_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  scatterMx <- pairs_scatterplotMatrix_aggBias(discrepDat)
  print(scatterMx)
  dev.off()

  png(sprintf("corrMx_spearman_discrep_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  corrMx <- pairs_corrMatrix_aggBias(discrepDat)
  print(corrMx)
  dev.off()

  ## discrepancy magnitude data ##
  png(sprintf("scatterMx_discrepMag_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  scatterMx <- pairs_scatterplotMatrix_aggBias(discrepMagDat)
  print(scatterMx)
  dev.off()

  # full correlation matrix
  png(sprintf("corrMx_spearman_discrepMag_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  corrMx <- pairs_corrMatrix_aggBias(discrepMagDat)
  print(corrMx)
  dev.off()
  
} # end pairwise

