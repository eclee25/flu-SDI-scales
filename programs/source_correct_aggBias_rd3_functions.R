setwd(dirname(sys.frame(1)$ofile))


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
  searchDir <- paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/correct_aggBias_rd3/") 
  return(searchDir)
}
################################


#### import data ################################
import_posteriorSamples_data_st <- function(modCodeStr){
  print(match.call())
  # for 11 series models

  idDat <- read_csv(string_modData_fname(modCodeStr), col_types = cols_only("fips_st" = "c", "season" = "d", "y" = "d", "y1" = "d", "pop" = "d")) %>%
    dplyr::rename(db_obs = y, aggBias_obs = y1)
  inDat <- read_csv(string_posteriorSamples_fname(modCodeStr)) 

  aggBias_samples <- inDat %>% select(contains("Predictor:"))
  obs_samples_df <- tbl_df(bind_cols(idDat, tbl_df(t(aggBias_samples))))

  # intercept_samples <- inDat %>% select(intercept_nonzero) %>% unlist
  # beta_samples <- inDat %>% select(-contains("Predictor."), -intercept_nonzero) %>% unlist

  return(list(obs_samples_df = obs_samples_df))
}


#### calculate new cty bias ################################
calculate_new_cty_burden_mag <- function(postSamplesLs_st, ctyObs){
  print(match.call())

  ## 5/29/18 checked that sum of county populations is equivalent to state population
  ctyObs_sub <- ctyObs %>%
    mutate(fips_st = substring(fips, 1, 2)) %>%
    select(season, fips, fips_st, pop)


  ## redistribute by population after transforming posterior samples back to original scale (magnitude db measures)
  samp_df <- postSamplesLs_st[["obs_samples_df"]] %>%
    rename(pop_st = pop) %>%
    full_join(ctyObs_sub, by = c("season", "fips_st")) %>%
    mutate(prop_stPop = pop/pop_st) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs((exp(.)-1)*prop_stPop))

  ## import inflow data
  inflowDat <- process_commuting_flows() %>% dplyr::select(-pop, -net_flow)
  samp_df2 <- left_join(samp_df, inflowDat, by = c("fips")) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs(. + netinflow_prop*.))

  ## calculate quantiles for intensity redistributed by population
  samp_summ <- samp_df2 %>% select(contains("V")) 
  q_5 <- apply(samp_summ, 1, quantile, probs = c(.5), na.rm = TRUE)
  q_025 <- apply(samp_summ, 1, quantile, probs = c(.025), na.rm = TRUE)
  q_975 <- apply(samp_summ, 1, quantile, probs = c(.975), na.rm = TRUE)
  
  cty_star_df <- samp_df2 %>%
    select(-contains("V")) %>%    
    dplyr::mutate(q_5 = q_5, q_025 = q_025, q_975 = q_975) %>%
    dplyr::filter(fips_st != "11")
  

  # # clean data for cty_star estimation
  # aggBias_samp_mx <- aggBias_samp_df %>% select(num_range("V", 1:200)) %>% as.matrix
  # # View(aggBias_samp_mx)

  # st_obs_ls <- aggBias_samp_df %>% select(obs_y_st) %>% unlist
  # st_obs_mx <- matrix(rep(st_obs_ls, ncol(aggBias_samp_mx)), byrow = FALSE, ncol = ncol(aggBias_samp_mx))
  # # View(st_obs_mx)

  # # clean data for cty_star validation
  # pred_ls <- aggBias_samp_df %>% select(corrFactor) %>% unlist
  # # print(head(pred_ls))

  # beta_samp_ls <- postSamplesLs[["beta_samp_ls"]]
  # beta_samp_mx <- matrix(rep(beta_samp_ls, length(pred_ls)), byrow = TRUE, ncol = length(beta_samp_ls))
  # # View(beta_samp_mx)

  # intercept_samp_ls <- postSamplesLs[["intercept_samp_ls"]]
  # intercept_samp_mx <- matrix(rep(intercept_samp_ls, length(pred_ls)), byrow = TRUE, ncol = length(intercept_samp_ls))
  # # View(intercept_samp_mx)

  # # calculation
  # cty_star2 <- st_obs_mx - aggBias_samp_mx

  # # validation 
  # cty_star <- st_obs_mx - (beta_samp_mx * pred_ls) - intercept_samp_mx

  # names(cty_star) <- paste0("V", ncol(aggBias_samp_mx))
  # # not sure why cty_star and cty_star2 are different

  # cty_star_mean <- apply(cty_star, 1, mean, na.rm=TRUE)
  # cty_star_sd <- apply(cty_star, 1, sd, na.rm=TRUE)
  # cty_star_q025 <- apply(cty_star, 1, quantile, .025, na.rm=TRUE)
  # cty_star_q5 <- apply(cty_star, 1, quantile, .5, na.rm=TRUE)
  # cty_star_q975 <- apply(cty_star, 1, quantile, .975, na.rm=TRUE)
  # cty_star_summ <- data.frame(ctyStar_mean = cty_star_mean, ctyStar_sd = cty_star_sd, ctyStar_q025 = cty_star_q025, ctyStar_q5 = cty_star_q5, ctyStar_q975 = cty_star_q975)

  # # return new dataframes with identifiers
  # cty_star_df <- aggBias_samp_df %>%
  #   select(-num_range("V", 1:200)) %>%
  #   bind_cols(tbl_df(cty_star))

  # cty_star_df_summ <- aggBias_samp_df %>%
  #   select(-num_range("V", 1:200)) %>%
  #   bind_cols(cty_star_summ)

  return(cty_star_df)
}

process_commuting_flows <- function(filename = "../reference_data/US_countyCommuter_fullData.csv"){
  
  origDat <- read_csv(filename, col_types = "ccdddd______") %>%
    dplyr::mutate(net_12 = commuters_12 - commuters_21) %>%
    dplyr::mutate(net_21 = commuters_21 - commuters_12)
  
  ## calculate net county inflows
  dat_into1 <- origDat %>%
    group_by(fips1) %>%
    summarise(net_flow = sum(net_21), pop = first(pop1)) %>%
    dplyr::rename(fips = fips1)
  dat_into2 <- origDat %>%
    group_by(fips2) %>%
    summarise(net_flow = sum(net_12), pop = first(pop2)) %>%
    dplyr::rename(fips = fips2)
  inflowDat <- bind_rows(dat_into1, dat_into2) %>%
    group_by(fips) %>%
    summarise(net_flow = sum(net_flow), pop = first(pop)) %>%
    dplyr::mutate(netinflow_prop = net_flow/pop) 

  return(inflowDat)

}
