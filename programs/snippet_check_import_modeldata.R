require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "8f_wksToEpi_v2-8"
modCodeStr_st <- "10f_wksToEpi_v1-6"

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
path_latlon_reg <- paste0(getwd(), "/region_latlon.csv")
path_region_cw <- paste0(getwd(), "/state_abbreviations_FIPS_region.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_response_reg <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))


# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_latlon_st = path_latlon_st,
                  path_latlon_reg = path_latlon_reg,
                  path_region_cw = path_region_cw,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty,
                  path_response_st = path_response_st,
                  path_response_reg = path_response_reg,
                  path_graphIdx_cty = path_graphIdx_cty,
                  path_graphExport_st = path_graphExport_st,
                  path_graphIdx_st = path_graphIdx_st,
                  path_fullIndic_cty = path_fullIndic_cty)

################################
# test1 <- import_obsFit_wksToEpi(modCodeStr, path_list)
# test2 <- import_obsFit_wksToEpi_st(modCodeStr_st, path_list) 
# test3 <- import_obsFit_wksToEpi_ctySt(modCodeStr, modCodeStr_st, FALSE, path_list)
# test4 <- import_obs_wksToEpi_reg(path_list)
# test5 <- import_obs_wksToEpi_ctyReg(modCodeStr, TRUE, path_list)

# test1 <- import_obs_wksToPeak(path_list)
# test2 <- import_obs_wksToPeak_st(path_list) 
# test3 <- import_obs_wksToPeak_ctySt(FALSE, path_list)
# test4 <- import_obs_wksToPeak_reg(path_list)
# test5 <- import_obs_wksToPeak_ctyReg(TRUE, path_list)

# test1 <- import_obs_iliEarly(path_list)
# test2 <- import_obs_iliEarly_st(path_list) 
# test3 <- import_obs_iliEarly_ctySt(FALSE, path_list)
# test4 <- import_obs_iliEarly_reg(path_list)
# test5 <- import_obs_iliEarly_ctyReg(TRUE, path_list)

# test1 <- import_obs_iliPeak(path_list)
# test2 <- import_obs_iliPeak_st(path_list) 
# test3 <- import_obs_iliPeak_ctySt(FALSE, path_list)
# test4 <- import_obs_iliPeak_reg(path_list)
# test5 <- import_obs_iliPeak_ctyReg(TRUE, path_list)