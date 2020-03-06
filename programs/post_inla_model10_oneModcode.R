## Name: Elizabeth Lee
## Date: 12/20/16
## Function: post-processing program for inla_model8, for a set of models with the same structure: plot coef distributions; includes state_passenger covariance mx interpretation
## Filenames: 
## Data Source: IMS Health
## Notes: need to SSH into snow server
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
require(readr)
require(ncf)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaData.R") # plot_coefDistr_season function within importPlot_coefDistr...
source("source_export_inlaData_st.R") # plot_stateChoro function
source("source_export_inlaData_hurdle.R") # importPlot_coefDistr_season_hurdle function
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function
# source("source_clean_response_functions_cty.R") # cty response functions

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- c(3:9)
modCodeStrLs <- sprintf("10f_wksToEpi_v%s-6", 1:2)
likString <- "poisson"; likStrings <- c(likString) 
source("source_calculate_residuals.R") # calculate_residuals function (source_calculate_residuals_shift1.R for iliSum; source_calculate_residuals.R for epiDur, wksToEpi)

#### IMPORT FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)

for (i in 1:length(modCodeStrLs)){
  # seasons <- seasLs[[i]]
  modCodeStr <- modCodeStrLs[i]
  
  #### EXPORT FILEPATHS #################################
  # diagnostic plot export directories
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
  path_plotExport <- getwd()
  path_plotExport_coefDistr <- paste0(path_plotExport, sprintf("/coefDistr_%s_", modCodeStr))
  
  # csv file export directories
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport <- getwd()
  path_csvExport_ids <- paste0(path_csvExport, "/ids_", modCodeStr, ".csv")
  
  #### results across seasons #################################
  # coef distributions by season
  importPlot_coefDistr_RV_spatiotemporal(path_csvExport, path_plotExport_coefDistr)

  #### diagnostics across seasons #################################

  ### model fit ###
  if ("gamma" %in% likStrings | "normal" %in% likStrings | "poisson" %in% likStrings){

    # scatter: predicted vs. observed data (yhat - nonzero) + 95%CI vs. y nonzero observed
    path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_st_spatiotemporal(path_csvExport, path_plotExport_predVsObs, likString, "y1", "mean", TRUE)

    # scatter: standardized residuals vs. predicted (yhat - nonzero model only)
    path_plotExport_residVsPred <- paste0(path_plotExport, sprintf("/diag_residVsPred_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_st_spatiotemporal(path_csvExport, path_plotExport_residVsPred, likString, "mean", "yhat_resid", FALSE)

    # scatter: standardized residuals vs. observed y_nonzero (yhat - nonzero model only)
    path_plotExport_residVsObs <- paste0(path_plotExport, sprintf("/diag_residVsObs_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_st_spatiotemporal(path_csvExport, path_plotExport_residVsObs, likString, "y1", "yhat_resid", FALSE)

    # scatter: predicted SD vs. predicted
    path_plotExport_predsdVsPred <- paste0(path_plotExport, sprintf("/diag_predsdVsPred_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_st_spatiotemporal(path_csvExport, path_plotExport_predsdVsPred, likString, "mean", "sd", FALSE)

    ## import summary statistics ##
    path_csvImport_estimates <- paste0(path_csvExport, sprintf("/summaryStats_%s.csv", modCodeStr))
    mod_est <- read_csv(path_csvImport_estimates, col_types = c("RV" = col_character())) %>%
      filter(likelihood == likString)


    ## map spatially structured state random effect terms ##
    if(nrow(mod_est %>% filter(effectType == "structured_st")) > 0){
      path_plotExport_strucEffects2 <- paste0(path_plotExport, sprintf("/choro_structuredEffect_st_%s.png", modCodeStr))
      idDat <- read_csv(path_csvExport_ids, col_types = cols_only(fips_st = "c", graphIdx_st = "c")) %>% distinct(fips_st, graphIdx_st)
      mod_est_strucEffects2 <- mod_est %>%
        filter(effectType == "structured_st") %>%
        clean_RVnames(.) %>%
        left_join(idDat, by = c("RV" = "graphIdx_st"))
      plot_stateChoro(path_plotExport_strucEffects2, path_abbr_st, mod_est_strucEffects2, "mean", "gradient", FALSE)
    }

  }

  #### diagnostics by season #################################
  for (s in seasons){
    print(paste("Season", s, "-----------------"))
  
    #### nonzero model figures ####
    if ("gamma" %in% likStrings | "normal" %in% likStrings | "poisson" %in% likStrings){
      path_csvImport_fittedNz <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_%s.csv", likString, modCodeStr))
      mod_nz_import <- read_csv(path_csvImport_fittedNz, col_types = cols(fips_st = col_character(), ID = col_character(), y1 = col_double())) %>%
        filter(season == s)
      mod_nz_fitted <- calculate_residuals(mod_nz_import, TRUE) # 2nd arg: nonzeroOnly
  
      if (nrow(mod_nz_fitted %>% filter(!is.na(y1))) > 0){
        # choropleth: observed values (y_nonzero) - Magnitude of non-zero epidemic
        path_plotExport_yobs_nz <- paste0(path_plotExport, sprintf("/choro_yObs_%s_S%s.png", modCodeStr, s))
        plot_stateChoro(path_plotExport_yobs_nz, path_abbr_st, mod_nz_fitted, "y1", "tier", FALSE)
  
        # choropleth: fitted values (yhat_i) - Magnitude of non-zero epidemic
        path_plotExport_yhat_nz <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
        plot_stateChoro(path_plotExport_yhat_nz, path_abbr_st, mod_nz_fitted, "mean", "tier", FALSE)

        # choropleth: standardized residuals
        path_plotExport_resid_nz <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
        plot_stateChoro(path_plotExport_resid_nz, path_abbr_st, mod_nz_fitted, "yhat_resid", "tier", FALSE)

      }
  
    }
  
  }
  
}






