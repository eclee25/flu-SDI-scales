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
source("source_export_inlaData_cty.R") # plot_countyChoro function
source("source_export_inlaData_hurdle.R") # importPlot_coefDistr_season_hurdle function
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function
source("source_clean_response_functions_cty.R") # cty response functions

#### set these! ################################
dbCodeStr <- "_iliRate" # _ilinDt_Octfit_span0.4_degree2
seasons <- c(3:9)

## seasLs for historical season sequence
# seasLs <- list(c(3:4,6:9), c(3:4,6,9), c(7,9), c(3,5:9), c(5,7:9), c(7,9), c(3:8), c(5:8), c(5:6), c(3:4,6:9), c(3:4,8:9), c(3:4), c(3:8), c(3:5,8), c(3,5), c(3:8), c(5:8), c(5,7), c(3:6,7:9), c(4:5,7,9), c(5,9), c(3:5,7:9), c(5,7:9), c(7:8), c(3,5:9), c(3,5:6,8), c(5:6)) 
# modCodeStrLs <- paste0(rep(c("8a_iliSum_v2-6_s6-", "8a_iliSum_v2-6_s4-", "8a_iliSum_v2-6_s2-"), 5), c(rep(5,3), rep(6,3), rep(7,3), rep(8,3), rep(9,3)))

## groupings for 8a v2 regions
# labLs <- c("1&2&3", "4&6", "5&7")
# modCodeStrLs <- paste0("8a_iliSum_v2-6_R", labLs)

# modCodeStrLs <- paste0("8f_wksToEpi_v", 1:2, "-2")
modCodeStrLs <- c("8g_iliRate_v2-1")

likString <- "normal"; likStrings <- c(likString)
source("source_calculate_residuals_shift1.R") # calculate_residuals function (source_calculate_residuals_shift1.R for iliSum; source_calculate_residuals.R for epiDur, wksToEpi)

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

  ### model validity ###
  if ("binomial" %in% likStrings){
    # scatter: predicted vs. observed data (phat - binomial) + 95%CI vs. y observed
    path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s_%s.png", "binomial", modCodeStr))
    plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predVsObs, "binomial", "y", "mean", TRUE)
  }

  ### model fit ###
  if ("gamma" %in% likStrings | "normal" %in% likStrings | "poisson" %in% likStrings){
    # 1/8/17 only coded up for iliSum response
    # # correlogram: Moran's I vs. distance
    # path_plotExport_correlogram <- paste0(path_plotExport, sprintf("/diag_correlog_%s_%s", likString, modCodeStr))
    # importPlot_correlogram(path_csvExport, path_plotExport_correlogram, path_list, likString)

    # scatter: predicted vs. observed data (yhat - nonzero) + 95%CI vs. y nonzero observed
    path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predVsObs, likString, "y1", "mean", TRUE)

    # scatter: standardized residuals vs. predicted (yhat - nonzero model only)
    path_plotExport_residVsPred <- paste0(path_plotExport, sprintf("/diag_residVsPred_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_residVsPred, likString, "mean", "yhat_resid", FALSE)

    # # scatter: raw residuals vs. predicted (yhat - nonzero model only)
    # path_plotExport_residVsPred2 <- paste0(path_plotExport, sprintf("/diag_rawresidVsPred_%s_%s.png", likString, modCodeStr))
    # plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_residVsPred2, likString, "mean", "yhat_rawresid", FALSE)

    # scatter: standardized residuals vs. observed y_nonzero (yhat - nonzero model only)
    path_plotExport_residVsObs <- paste0(path_plotExport, sprintf("/diag_residVsObs_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_residVsObs, likString, "y1", "yhat_resid", FALSE)

    # # scatter: raw residuals vs. observed y_nonzero (yhat - nonzero model only)
    # path_plotExport_residVsObs2 <- paste0(path_plotExport, sprintf("/diag_rawresidVsObs_%s_%s.png", likString, modCodeStr))
    # plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_residVsObs2, likString, "y1", "yhat_rawresid", FALSE)

    # scatter: predicted SD vs. predicted
    path_plotExport_predsdVsPred <- paste0(path_plotExport, sprintf("/diag_predsdVsPred_%s_%s.png", likString, modCodeStr))
    plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predsdVsPred, likString, "mean", "sd", FALSE)

    # # scatter: predicted SD vs. observed y_nonzero
    # path_plotExport_predsdVsObs <- paste0(path_plotExport, sprintf("/diag_predsdVsObs_%s_%s.png", likString, modCodeStr))
    # plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predsdVsObs, likString, "y1", "sd", FALSE)

    # # scatter: predicted SD vs. raw residuals
    # path_plotExport_predsdVsResid2 <- paste0(path_plotExport, sprintf("/diag_predsdVsRawresid_%s_%s.png", likString, modCodeStr))
    # plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predsdVsResid2, likString, "yhat_rawresid", "sd", FALSE)

    # # scatter: predicted SD vs. standardized residuals
    # path_plotExport_predsdVsResid <- paste0(path_plotExport, sprintf("/diag_predsdVsResid_%s_%s.png", likString, modCodeStr))
    # plot_diag_scatter_hurdle_spatiotemporal(path_csvExport, path_plotExport_predsdVsResid, likString, "yhat_resid", "sd", FALSE)

    ## import summary statistics ##
    path_csvImport_estimates <- paste0(path_csvExport, sprintf("/summaryStats_%s.csv", modCodeStr))
    mod_est <- read_csv(path_csvImport_estimates, col_types = c("RV" = col_character())) %>%
      filter(likelihood == likString)

    ## map county random effect terms ##
    path_plotExport_ctyEffects <- paste0(path_plotExport, sprintf("/choro_spatialEffect_%s.png", modCodeStr))
    mod_est_ctyEffects <- mod_est %>%
      filter(effectType == "spatial") %>%
      dplyr::rename(fips = RV)
    plot_countyChoro(path_plotExport_ctyEffects, mod_est_ctyEffects, "mean", "gradient", FALSE)

    ## map spatially structured county random effect terms ##
    if(nrow(mod_est %>% filter(effectType == "structured")) > 0){
      path_plotExport_strucEffects <- paste0(path_plotExport, sprintf("/choro_structuredEffect_%s.png", modCodeStr))
      idDat <- read_csv(path_csvExport_ids, col_types = cols_only(fips = "c", graphIdx = "c")) %>% distinct(fips, graphIdx)
      mod_est_strucEffects <- mod_est %>%
        filter(effectType == "structured") %>%
        clean_RVnames(.) %>%
        left_join(idDat, by = c("RV" = "graphIdx"))
      plot_countyChoro(path_plotExport_strucEffects, mod_est_strucEffects, "mean", "gradient", FALSE)
    }

    ## map spatially structured state random effect terms ##
    if(nrow(mod_est %>% filter(effectType == "structured_st")) > 0){
      path_plotExport_strucEffects2 <- paste0(path_plotExport, sprintf("/choro_structuredEffect_st_%s.png", modCodeStr))
      idDat <- read_csv(path_csvExport_ids, col_types = cols_only(fips = "c", graphIdx_st = "c")) %>% distinct(fips, graphIdx_st)
      mod_est_strucEffects2 <- mod_est %>%
        filter(effectType == "structured_st") %>%
        clean_RVnames(.) %>%
        left_join(idDat, by = c("RV" = "graphIdx_st"))
      plot_countyChoro(path_plotExport_strucEffects2, mod_est_strucEffects2, "mean", "gradient", FALSE)
    }

  }

  #### diagnostics by season #################################
  for (s in seasons){
    print(paste("Season", s, "-----------------"))

    #### binomial model figures ####
    if ("binomial" %in% likStrings){
      ## fitted outputs ##
      path_csvImport_fittedBinomial <- paste0(path_csvExport, sprintf("/summaryStatsFitted_binomial_%s.csv", modCodeStr))
      mod_bin_fitted <- read_csv(path_csvImport_fittedBinomial, col_types = c("fips" = col_character(), "ID" = col_character())) %>%
        filter(season == s)

      # choropleth: fitted values (phat_i) -> Prob(epidemic)
      path_plotExport_phat_bin <- paste0(path_plotExport, sprintf("/choro_pHat_%s_S%s.png", modCodeStr, s))
      plot_countyChoro(path_plotExport_phat_bin, mod_bin_fitted, "mean", "gradient", FALSE)

      # choropleth: SD of fitted values (phat_i)
      path_plotExport_phatSD_bin <- paste0(path_plotExport, sprintf("/choro_pHatSD_%s_S%s.png", modCodeStr, s))
      plot_countyChoro(path_plotExport_phatSD_bin, mod_bin_fitted, "sd", "gradient", FALSE)

    }

    #### nonzero model figures ####
    if ("gamma" %in% likStrings | "normal" %in% likStrings | "poisson" %in% likStrings){
      path_csvImport_fittedNz <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_%s.csv", likString, modCodeStr))
      mod_nz_import <- read_csv(path_csvImport_fittedNz, col_types = cols(fips = col_character(), ID = col_character(), y1 = col_double())) %>%
        filter(season == s)
      mod_nz_fitted <- calculate_residuals(mod_nz_import, TRUE) # 2nd arg: nonzeronOnly

      if (nrow(mod_nz_fitted %>% filter(!is.na(y1))) > 0){
        # choropleth: observed values (y_nonzero) - Magnitude of non-zero epidemic
        path_plotExport_yobs_nz <- paste0(path_plotExport, sprintf("/choro_yObs_%s_S%s.png", modCodeStr, s))
        plot_countyChoro(path_plotExport_yobs_nz, mod_nz_fitted, "y1", "tier", TRUE)

        # choropleth: fitted values (yhat_i) - Magnitude of non-zero epidemic
        path_plotExport_yhat_nz <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
        plot_countyChoro(path_plotExport_yhat_nz, mod_nz_fitted, "mean", "tier", FALSE)

        # # choropleth: SD of fitted values (yhat_i)
        # path_plotExport_yhatSD_nz <- paste0(path_plotExport, sprintf("/choro_yHatSD_%s_S%s.png", modCodeStr, s))
        # plot_countyChoro(path_plotExport_yhatSD_nz, mod_nz_fitted, "sd", "gradient", FALSE)

        # choropleth: standardized residuals
        path_plotExport_resid_nz <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
        plot_countyChoro(path_plotExport_resid_nz, mod_nz_fitted, "yhat_resid", "tier", FALSE)

        # # choropleth: raw residuals
        # path_plotExport_resid_nz2 <- paste0(path_plotExport, sprintf("/choro_yRawResid_%s_S%s.png", modCodeStr, s))
        # plot_countyChoro(path_plotExport_resid_nz2, mod_nz_fitted, "yhat_rawresid", "tier", FALSE)
      }

    }

  }

}






