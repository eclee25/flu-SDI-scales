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
source("source_clean_response_functions_aggBias.R") # import_fit_aggBias_seasIntensityRR
source("source_clean_data_functions.R")
source("source_prepare_inlaData_aggBias.R") # functions to import aggbias data
source("source_prepare_inlaData_cty.R") # functions to import response data from original models
source("source_export_inlaDiagnostics.R") # import fitted data
source("source_calculate_residuals.R")

#### set these! ####################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
likString <- "normal"
modCodeStr <- "11a_iliSum_v2-2"

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

mod_list <- list(cty = "8a_iliSum_v2-6", 
                 st = "10a_iliSum_v1-2")

#### import data ####################################
modDat <- model11a_iliSum_v7(path_list, mod_list) 

fitDat <- import_diag_data_residuals(path_csvExport, likString)

if (modCodeStr == "11a_iliSum_v2-1"){
     #### check features of data points that had large residuals ####################################
     View(fitDat %>% filter(season==4 & yhat_resid > 3)) # VA,WA,MD
     View(fitDat %>% filter(season==6 & yhat_resid > 2.5)) # MA,MT
     View(fitDat %>% filter(season==7 & yhat_resid > 2.5)) # FL,ID,MT

     # How many counties in VA? # 131 of 134 have large residuals
     fitDat %>% filter(season==4 & st == "VA") %>% count
     fitDat %>% filter(season==4 & yhat_resid > 3 & st == "VA") %>% count 

     # How many counties in MA? # 14 of 14 have large residuals
     fitDat %>% filter(season==6 & st == "MA") %>% count
     fitDat %>% filter(season==6 & yhat_resid > 2.5 & st == "MA") %>% count

     # How many counties in MT? # 49 of 56 have large residuals in S6
     fitDat %>% filter(season==6 & st == "MT") %>% count
     fitDat %>% filter(season==6 & yhat_resid > 2.5 & st == "MT") %>% count

     # How many counties in FL? # 48 of 67 have large residuals
     fitDat %>% filter(season==7 & st == "FL") %>% count
     fitDat %>% filter(season==7 & yhat_resid > 2.5 & st == "FL") %>% count

     # How many counties in MT in S7? # 34 of 56 have large residuals
     fitDat %>% filter(season==7 & st == "MT") %>% count
     fitDat %>% filter(season==7 & yhat_resid > 2.5 & st == "MT") %>% count

     ####  ####################################
     # It seems that the feature binding these observations together is logy1_st == NA
     View(modDat %>% filter(season==4 & is.na(logy1_st)) %>% select(-contains("X_"), -contains("O_")))
     View(modDat %>% filter(season==6 & is.na(logy1_st)) %>% select(-contains("X_"), -contains("O_")))
     View(modDat %>% filter(season==7 & is.na(logy1_st)) %>% select(-contains("X_"), -contains("O_")))

     } else if (modCodeStr == "11a_iliSum_v2-2"){
     # 7/20: I didn't see any clear patterns for why some states had larger residuals, but overall the residuals have gotten smaller and less distinctly grouped.

     #### check features of data points that had large residuals ####################################
     View(fitDat %>% filter(season==4 & abs(yhat_resid) > 2)) # +CO,-NH,-OR,-VT,-WY
     View(fitDat %>% filter(season==5 & abs(yhat_resid) > 1.25)) # -CA,-ID,-OR,+GA
     View(fitDat %>% filter(season==8 & yhat_resid > 2)) # NH,OR,VT

     # How many counties in ID? # 32 of 44 have large residuals
     fitDat %>% filter(season==5 & st == "ID") %>% count
     fitDat %>% filter(season==5 & abs(yhat_resid) > 1.25 & st == "ID") %>% count 

     # How many counties in NH? # 10 of 10 have large residuals
     fitDat %>% filter(season==4 & st == "NH") %>% count
     fitDat %>% filter(season==4 & abs(yhat_resid) > 2 & st == "NH") %>% count
     fitDat %>% filter(season==8 & abs(yhat_resid) > 2 & st == "NH") %>% count 
     
     # How many counties in OR? # 10,33,23 of 36 have large residuals
     fitDat %>% filter(season==4 & st == "OR") %>% count
     fitDat %>% filter(season==4 & abs(yhat_resid) > 2 & st == "OR") %>% count
     fitDat %>% filter(season==5 & abs(yhat_resid) > 1.25 & st == "OR") %>% count
     fitDat %>% filter(season==8 & abs(yhat_resid) > 2 & st == "OR") %>% count 
}

