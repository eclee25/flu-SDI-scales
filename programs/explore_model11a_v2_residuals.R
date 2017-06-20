## Name: Elizabeth Lee
## Date: 7/15/17
## Function: Why are the residuals separated into two groups? 
## Filenames: 
## Data Source: IMS Health 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


#### header ####################################
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

source("source_clean_response_functions_cty.R") # cleanR_iliSum_shift1_cty_aggBias
source("source_clean_response_functions_st.R") # cleanR_iliSum_shift1_st_aggBias
source("source_clean_data_functions.R")
source("source_prepare_inlaData_aggBias.R") # functions to import aggbias data
source("source_prepare_inlaData_cty.R") # functions to import response data from original models
source("source_export_inlaDiagnostics.R") # import fitted data
source("source_calculate_residuals_shift1.R")

#### set these! ####################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
likString <- "normal"
modCodeStr <- "11a_iliSum_v2-1"

#### FILEPATHS #################################
# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

setwd(dirname(sys.frame(1)$ofile))
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
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))

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
                    path_graphIdx_st = path_graphIdx_st)


#### import data ####################################
modDat <- model8a_iliSum_v7(path_list) %>%
    mutate(seasIntensityRR = y1-logE)

fitDat <- import_diag_data_residuals(path_csvExport, likString)


