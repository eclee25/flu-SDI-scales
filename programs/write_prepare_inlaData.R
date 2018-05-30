
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
dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
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
source("source_import_modeldata.R")

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
# export data
setwd(dirname(sys.frame(1)$ofile))
dir.create("../R_export/inlaModelData_import", showWarnings = FALSE)
path_dataExport <- paste0(getwd(), "/../R_export/inlaModelData_import/")

# export figures
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/inlaModelData_import", showWarnings = FALSE)
path_graphExport <- paste0(getwd(), "/../graph_outputs/explore_inlaModelData_import/")

#### MAIN #################################
#### county level data ####
mod_8fV7 <- model8f_wksToEpi_v7(path_list) 
mod_8iV7 <- model8i_wksToPeak_v7(path_list)
mod_8hV7 <- model8h_iliEarly_v7(path_list)
mod_8bV7 <- model8b_iliPeak_v7(path_list)

# fnames
fname_8f <- "inlaImport_model8f_wksToEpi_irDt_log_v7"
fname_8i <- "inlaImport_model8i_wksToPeak_irDt_log_v7"
fname_8h <- "inlaImport_model8h_iliEarly_irDt_v7"
fname_8b <- "inlaImport_model8b_iliPeak_irDt_v7"

# write_csv(mod_8fV7, paste0(path_dataExport, fname_8f, ".csv"))
# write_csv(mod_8iV7, paste0(path_dataExport, fname_8i, ".csv"))
# write_csv(mod_8hV7, paste0(path_dataExport, fname_8h, ".csv"))
# write_csv(mod_8bV7, paste0(path_dataExport, fname_8b, ".csv"))

##################################################################
#### state level data ####
mod_10bV2 <- model10b_iliPeak_irDt_v2(path_list) 
mod_10hV2 <- model10h_iliEarly_irDt_v2(path_list)
mod_10aV2 <- model10a_iliSum_irDt_v2(path_list)

# fnames
fname_10b <- "inlaImport_model10b_iliPeak_irDt_v7"
fname_10h <- "inlaImport_model10h_iliEarly_irDt_v7"
fname_10a <- "inlaImport_model10a_iliSum_irDt_v7"

write_csv(mod_10bV2, paste0(path_dataExport, fname_10b, ".csv"))
write_csv(mod_10hV2, paste0(path_dataExport, fname_10h, ".csv"))
write_csv(mod_10aV2, paste0(path_dataExport, fname_10a, ".csv"))

#### aggregation bias data ####
mod_11fV7 <- mod_8fV7 %>%
  left_join(import_obs_wksToEpi_ctySt(FALSE, path_list) %>%
    select(season, fips, obs_diff_stCty), by = c("season", "fips")) %>%
  rename(y1_cty_obs = y1) %>%
  rename(y1 = obs_diff_stCty)
mod_11iV7 <- mod_8iV7 %>%
  left_join(import_obs_wksToPeak_ctySt(FALSE, path_list) %>%
    select(season, fips, obs_diff_stCty), by = c("season", "fips")) %>%
  rename(y1_cty_obs = y1) %>%
  rename(y1 = obs_diff_stCty)
mod_11hV7 <- mod_8hV7 %>%
  left_join(import_obs_iliEarly_ctySt(TRUE, path_list) %>%
    select(season, fips, obs_diff_stCty), by = c("season", "fips")) %>%
  rename(y1_cty_obs = y1) %>%
  rename(y1 = obs_diff_stCty)
mod_11bV7 <- mod_8bV7 %>%
  left_join(import_obs_iliPeak_ctySt(TRUE, path_list) %>%
    select(season, fips, obs_diff_stCty), by = c("season", "fips")) %>%
  rename(y1_cty_obs = y1) %>%
  rename(y1 = obs_diff_stCty)

# fnames
fname_11f <- "inlaImport_model11f_wksToEpi_irDt_v7"
fname_11i <- "inlaImport_model11i_wksToPeak_irDt_v7"
fname_11h <- "inlaImport_model11h_iliEarly_irDt_v7"
fname_11b <- "inlaImport_model11b_iliPeak_irDt_v7"

# write_csv(mod_11fV7, paste0(path_dataExport, fname_11f, ".csv"))
# write_csv(mod_11iV7, paste0(path_dataExport, fname_11i, ".csv"))
# write_csv(mod_11hV7, paste0(path_dataExport, fname_11h, ".csv"))
# write_csv(mod_11bV7, paste0(path_dataExport, fname_11b, ".csv"))

##################################################################
#### plot response data ####
plot_response <- function(inlaData, fname){
  dbPlot <- ggplot(inlaData, aes(x = y1, group = season)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density() +
    facet_wrap(~season, scales = "free")
  ggsave(fname, dbPlot, width = 6, height = 4, dpi = 300)
}

# plot_response(mod_8fV7, paste0(path_graphExport, fname_8f, ".png"))
# plot_response(mod_8iV7, paste0(path_graphExport, fname_8i, ".png"))
# plot_response(mod_8hV7, paste0(path_graphExport, fname_8h, ".png"))
# plot_response(mod_8bV7, paste0(path_graphExport, fname_8b, ".png"))

# plot_response(mod_11fV7, paste0(path_graphExport, fname_11f, ".png"))
# plot_response(mod_11iV7, paste0(path_graphExport, fname_11i, ".png"))
# plot_response(mod_11hV7, paste0(path_graphExport, fname_11h, ".png"))
# plot_response(mod_11bV7, paste0(path_graphExport, fname_11b, ".png"))


