
## Name: Elizabeth Lee
## Date: 7/5/17
## Function: Model 10f v1: state-level iid spatial model, weeks to epidemic start (from ISO weeknum 40) response variable, with interaction terms
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA) # main dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies


modCodeLs <- c("10f_wksToEpi_v2-6")

for (i in 1:length(modCodeLs)){
  
  #### set these! ################################
  dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
  modCodeStr <- modCodeLs[i] # modCodeStr <- "8a_iliSum_v2-6_c60-3"
  # keep <- keepLs[i]
  # set.seed(seedLs[i])
  rdmFx_RV <- "phi"
  likString <- "poisson"
  origin_locations_file <- "st"
  dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
  s <- 999 # all seasons code for spatiotemporal analysis = 999
  
  #### SOURCE: clean and import model data #################################
  setwd(dirname(sys.frame(1)$ofile))
  source("source_clean_response_functions_cty.R") # clean_graphIdx
  source("source_clean_data_functions.R") # state and region covariate data
  source("source_prepare_inlaData_st.R") # functions to aggregate all data sources for model
  source("source_clean_response_functions_st.R") # state response
  source("source_clean_data_functions_st.R") # state model only covariate data
  source("source_export_inlaData.R") # functions to plot general model diagnostics
  source("source_export_inlaData_hurdle.R") # data export functions for hurdle model
  source("source_pp_checks.R")
  
  #### FILEPATHS #################################
  setwd('../reference_data')
  path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
  path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
  
  setwd('./stateFlightpassenger_graph')
  path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
  path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")
  
  setwd("../../R_export")
  path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
  path_fullIndic_st <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_st.csv", dbCodeStr))

  # put all paths in a list to pass them around in functions
  path_list <- list(path_abbr_st = path_abbr_st,
                    path_latlon_st = path_latlon_st,
                    path_response_st = path_response_st, 
                    path_graphExport_st = path_graphExport_st,
                    path_graphIdx_st = path_graphIdx_st,
                    path_fullIndic_st = path_fullIndic_st)
  
  #### MAIN #################################
  #### Import and process data ####
  modData_full <- model10f_wksToEpi_v2(path_list)
  
  stPassenger_adjMx <- clean_ctyCommmuter_stPassenger_graph(path_list, "state")

  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_st_nonzero, model = "iid") +
    f(graphIdx_st_nonzero, model = "besag", graph = stPassenger_adjMx) +
    f(regionID_nonzero, model = "iid") +
    f(season_nonzero, model = "iid") +
    intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + O_insured_nonzero + X_poverty_nonzero + X_child_nonzero + X_adult_nonzero + X_hospaccess_nonzero +    X_popdensity_nonzero + X_housdensity_nonzero + X_vaxcovI_nonzero + X_vaxcovE_nonzero +     X_H3A_nonzero + X_B_nonzero +     X_priorImmunity_nonzero + X_anomHumidity_nonzero + X_pollution_nonzero + X_singlePersonHH_nonzero  +X_latitude_nonzero
    
    # + X_sourceLocDist_nonzero + X_humidity_nonzero

  #### export formatting ####
  # diagnostic plot export directories
  setwd(dirname(sys.frame(1)$ofile))
  dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
  setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
  path_plotExport <- getwd()

  # csv file export directories
  setwd(dirname(sys.frame(1)$ofile))
  dir.create(sprintf("../R_export/inlaModelData_export/%s", modCodeStr), showWarnings = FALSE)
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport <- getwd()

  #### run models for all seasons ################################
  modData_hurdle <- convert_hurdleModel_nz_spatiotemporal_st(modData_full)

  mod <- inla(formula,
              family = "poisson",
              data = modData_hurdle,
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))),
              control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE, debug = TRUE)


  #### model summary outputs ################################
  # 7/20/16 reorganized

  #### write DIC and CPO values in separate tables by season ####
  # file path
  path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s.csv", modCodeStr))
  # DIC & CPO file formatting
  dicData <- unlist(c(modCodeStr, s, as.character(Sys.Date()), mod$dic$dic, sum(log(mod$cpo$cpo), na.rm=TRUE), sum(mod$cpo$failure, na.rm=TRUE), use.names=FALSE))
  dicData2 <- as.data.frame(matrix(dicData, nrow = 1), byrow = TRUE)
  names(dicData2) <- c("modCodeStr", "season", "exportDate", "DIC", "CPO", "cpoFail")
  # write DIC & CPO to file
  export_DIC(path_csvExport_dic, dicData2)

  #### write DIC and CPO for individual observations #### 
  # file path
  path_csvExport_cpoPIT <- paste0(path_csvExport, sprintf("/cpoPIT_observations_%s.csv", modCodeStr))
  # write CPO and PIT for each observation to file
  export_cpoPIT_observations(path_csvExport_cpoPIT, mod)

  #### write random and group effect identities ####
  # file path
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s.csv", modCodeStr))
  # write identity codes to file
  export_ids_st(path_csvExport_ids, modData_full)

  #### write fixed and random effects summary statistics ####
  # file path
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s.csv", modCodeStr))
  # write all summary statistics to file
  export_summaryStats_hurdle_likString(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s, likString) # assuming hyperpar, fixed always exist

  #### process fitted values for each model ################################
  # normal model processing
  path_csvExport_fittedNonzero <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_%s.csv", likString, modCodeStr))
  dummy_nz <- mod$summary.fitted.values[1:nrow(modData_full),]
  mod_nz_fitted <- export_summaryStats_fitted_hurdle_st(path_csvExport_fittedNonzero, dummy_nz, modData_full, modCodeStr, dbCodeStr, s)

  #### draw sample posteriors ################################
  path_csvExport_posteriorSamples <- paste0(path_csvExport, sprintf("/posteriorSamples_%s_%s.csv", likString, modCodeStr))
  export_posterior_samples(path_csvExport_posteriorSamples, mod)


  #### Diagnostic plots ################################

  #### normal likelihood figures ####
  # marginal posteriors: first 6 county random effects (nu or phi)
  path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_%s.png", modCodeStr, rdmFx_RV, likString))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$fips_st_nonzero, "nu")

  # marginal posteriors: first 6 observation error terms
  path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_ID1-6_marg_%s.png", modCodeStr, likString))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$ID_nonzero, "obs err")

}



