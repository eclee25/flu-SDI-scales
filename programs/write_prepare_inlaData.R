
## Name: Elizabeth Lee
## Date: 9/19/17
## Function: export input data for INLA models -- refactor code so that the data are imported from an existing dataframe instead of the database
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes:
##
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse)
require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
s <- 999 # all seasons code for spatiotemporal analysis = 999

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_prepare_inlaData_st.R") # functions to aggregate all data sources for model
source("source_clean_response_functions_st.R") # state response
source("source_clean_data_functions_st.R") # state model only covariate data

#### IMPORT FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

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

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty,
                  path_graphIdx_cty = path_graphIdx_cty,
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st,
                  path_graphExport_st = path_graphExport_st,
                  path_graphIdx_st = path_graphIdx_st,
                  path_fullIndic_cty = path_fullIndic_cty,
                   path_fullIndic_st = path_fullIndic_st)

#### EXPORT FILEPATHS #################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../R_export/inlaModelData_import", showWarnings = FALSE)
path_dataExport <- paste0(getwd(), "/../R_export/inlaModelData_import/")

#### MAIN #################################
#### county level data ####
mod_8fV7 <- model8f_wksToEpi_v7(path_list) 
mod_8iV7 <- model8i_wksToPeak_v7(path_list)
mod_8hV7 <- model8h_iliEarly_v7(path_list)
mod_8bV7 <- model8b_iliPeak_v7(path_list)

write_csv(mod_8fV7, paste0(path_dataExport, "inlaImport_model8f_wksToEpi_v7.csv"))
write_csv(mod_8iV7, paste0(path_dataExport, "inlaImport_model8i_wksToPeak_v7.csv"))
write_csv(mod_8hV7, paste0(path_dataExport, "inlaImport_model8h_iliEarly_v7.csv"))
write_csv(mod_8bV7, paste0(path_dataExport, "inlaImport_model8b_iliPeak_v7.csv"))

#### state level data ####
mod_10fV2 <- model10f_wksToEpi_v2(path_list)

write_csv(mod_10fV2, paste0(path_dataExport, "inlaImport_model10f_wksToEpi_v2.csv"))
