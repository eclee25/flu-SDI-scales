# for 11 series models only

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_msFigs.R") # season labels

#### path functions ################################
string_modData_fname <- function(modCodeStr){
  splitmod <- unlist(strsplit(modCodeStr, "_"))
  return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_import/inlaImport_model", splitmod[1], "_", splitmod[2], "_irDt_v7.csv"))
}
################################
string_posteriorSamples_fname <- function(modCodeStr){
  searchDir <- paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/") 
  return(grep("posteriorSamples_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}
################################
string_exportFig_folder <- function(){
  searchDir <- paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/correct_aggBias_rd2/") 
  return(searchDir)
}
################################


#### import data ################################
import_posteriorSamples_data <- function(modCodeStr){
  print(match.call())
  # for 11 series models

  idDat <- read_csv(string_modData_fname(modCodeStr), col_types = cols_only("fips" = "c", "fips_st" = "c", "season" = "d", "y" = "d", "y1" = "d")) %>%
    rename(db_obs = y, aggBias_obs = y1)

  inDat <- read_csv(string_posteriorSamples_fname(modCodeStr)) 

  aggBias_samples <- inDat %>% select(contains("Predictor:"))
  obs_samples_df <- tbl_df(bind_cols(idDat, tbl_df(t(aggBias_samples))))

  intercept_samples <- inDat %>% select(intercept_nonzero) %>% unlist
  beta_samples <- inDat %>% select(-contains("Predictor:"), -intercept_nonzero) %>% unlist

  return(list(obs_samples_df = obs_samples_df, intercept_samp_ls = intercept_samples, beta_samp_ls = beta_samples))
}
################################
grab_predictor_data <- function(modCodeStr, coefName){
  print(match.call())
  # grab predictor data information for correction factor

  inData <- read_csv(string_modData_fname(modCodeStr)) 
  predData <- inData %>%
    mutate(X_rnorm = rnorm(nrow(inData))) %>%
    select_("fips", "season", coefName) %>%
    rename_("corrFactor" = coefName)

  return(predData)
}
################################

################################

#### calculate new cty bias ################################
calculate_new_cty_burden <- function(postSamplesLs, stObs, predData){
  print(match.call())

  aggBias_samp_df <- postSamplesLs[["obs_samples_df"]] %>%
    full_join(stObs %>% select(-E) %>% rename(obs_y_st = obs_y), by = c("season", "fips_st")) %>%
    full_join(predData, by = c("season", "fips"))

  # clean data for cty_star estimation
  aggBias_samp_mx <- aggBias_samp_df %>% select(num_range("V", 1:200)) %>% as.matrix
  # View(aggBias_samp_mx)

  st_obs_ls <- aggBias_samp_df %>% select(obs_y_st) %>% unlist
  st_obs_mx <- matrix(rep(st_obs_ls, ncol(aggBias_samp_mx)), byrow = FALSE, ncol = ncol(aggBias_samp_mx))
  # View(st_obs_mx)

  # clean data for cty_star validation
  pred_ls <- aggBias_samp_df %>% select(corrFactor) %>% unlist
  # print(head(pred_ls))

  beta_samp_ls <- postSamplesLs[["beta_samp_ls"]]
  beta_samp_mx <- matrix(rep(beta_samp_ls, length(pred_ls)), byrow = TRUE, ncol = length(beta_samp_ls))
  # View(beta_samp_mx)

  intercept_samp_ls <- postSamplesLs[["intercept_samp_ls"]]
  intercept_samp_mx <- matrix(rep(intercept_samp_ls, length(pred_ls)), byrow = TRUE, ncol = length(intercept_samp_ls))
  # View(intercept_samp_mx)

  # calculation
  cty_star2 <- st_obs_mx - aggBias_samp_mx

  # validation 
  cty_star <- st_obs_mx - (beta_samp_mx * pred_ls) - intercept_samp_mx

  names(cty_star) <- paste0("V", ncol(aggBias_samp_mx))
  # not sure why cty_star and cty_star2 are different

  cty_star_mean <- apply(cty_star, 1, mean, na.rm=TRUE)
  cty_star_sd <- apply(cty_star, 1, sd, na.rm=TRUE)
  cty_star_q025 <- apply(cty_star, 1, quantile, .025, na.rm=TRUE)
  cty_star_q5 <- apply(cty_star, 1, quantile, .5, na.rm=TRUE)
  cty_star_q975 <- apply(cty_star, 1, quantile, .975, na.rm=TRUE)
  cty_star_summ <- data.frame(ctyStar_mean = cty_star_mean, ctyStar_sd = cty_star_sd, ctyStar_q025 = cty_star_q025, ctyStar_q5 = cty_star_q5, ctyStar_q975 = cty_star_q975)

  # return new dataframes with identifiers
  cty_star_df <- aggBias_samp_df %>%
    select(-num_range("V", 1:200)) %>%
    bind_cols(tbl_df(cty_star))

  cty_star_df_summ <- aggBias_samp_df %>%
    select(-num_range("V", 1:200)) %>%
    bind_cols(cty_star_summ)

  return(cty_star_df_summ)
}

#### plot county burden vs star ################################
scatter_cty_ctyStar <- function(ctyStarDat, pltFormats){
  print(match.call())

  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  measure <- pltFormats$measure; coefName <- substring(pltFormats$coefName, 2, nchar(pltFormats$coefName)) 

  plotDat <- ctyStarDat %>%
    left_join(season_labels(), by = c("season"))

  plt <- ggplot(plotDat, aes(x = db_obs, y = ctyStar_mean)) +
    geom_pointrange(aes(ymin = ctyStar_q025, ymax = ctyStar_q975)) +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    theme_bw() + 
    scale_x_continuous(paste("Observed", measure)) +
    scale_y_continuous(paste("Est. County Burden (95%CI) correct:", substring(coefName, 2, nchar(coefName)))) +
    facet_wrap(~seasLabs)

  exportFname <- paste0(string_exportFig_folder(), "scatter_ctyStar_", measure, coefName, ".png")
  ggsave(exportFname, plt, units = "in", w = w, h = h, dpi = dp)

  return(plt)
}