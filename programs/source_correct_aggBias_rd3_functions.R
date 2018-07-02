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


  ## redistribute by population 
  samp_df <- postSamplesLs_st[["obs_samples_df"]] %>%
    rename(pop_st = pop) %>%
    full_join(ctyObs_sub, by = c("season", "fips_st")) %>%
    mutate(prop_stPop = pop/pop_st) #%>%
    # mutate_at(.vars = vars(contains("V")), .funs = funs(.*prop_stPop)) # 

  ## import inflow data then transform posterior samples back to original scale (magnitude db measures)
  inflowDat <- process_commuting_flows() %>% dplyr::select(-pop, -net_flow)
  samp_df2 <- left_join(samp_df, inflowDat, by = c("fips")) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs(. + netinflow_prop*.)) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs((exp(.)-1)))

  ## calculate quantiles for intensity redistributed by population
  samp_summ <- samp_df2 %>% select(contains("V")) 
  q_5 <- apply(samp_summ, 1, quantile, probs = c(.5), na.rm = TRUE)
  q_025 <- apply(samp_summ, 1, quantile, probs = c(.025), na.rm = TRUE)
  q_975 <- apply(samp_summ, 1, quantile, probs = c(.975), na.rm = TRUE)
  q_sd <- apply(samp_summ, 1, sd, na.rm = TRUE)
  
  cty_star_df <- samp_df2 %>%
    select(-contains("V")) %>%    
    dplyr::mutate(q_5 = q_5, q_025 = q_025, q_975 = q_975, q_sd = q_sd) %>%
    dplyr::filter(fips_st != "11")

  return(cty_star_df)
}

calculate_new_cty_burden_timing <- function(postSamplesLs_st, ctyObs){
  print(match.call())

  ## 5/29/18 checked that sum of county populations is equivalent to state population
  ctyObs_sub <- ctyObs %>%
    mutate(fips_st = substring(fips, 1, 2)) %>%
    select(season, fips, fips_st, pop)


  ## redistribute by population 
  samp_df <- postSamplesLs_st[["obs_samples_df"]] %>%
    rename(pop_st = pop) %>%
    full_join(ctyObs_sub, by = c("season", "fips_st")) %>%
    mutate(prop_stPop = pop/pop_st) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs(.*prop_stPop)) # 

  ## import inflow data then transform posterior samples back to original scale (magnitude db measures)
  inflowDat <- process_commuting_flows() %>% dplyr::select(-pop, -net_flow)
  samp_df2 <- left_join(samp_df, inflowDat, by = c("fips")) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs(. + netinflow_prop*.)) %>%
    mutate_at(.vars = vars(contains("V")), .funs = funs((exp(.))))

  ## calculate quantiles for intensity redistributed by population
  samp_summ <- samp_df2 %>% select(contains("V")) 
  q_5 <- apply(samp_summ, 1, quantile, probs = c(.5), na.rm = TRUE)
  q_025 <- apply(samp_summ, 1, quantile, probs = c(.025), na.rm = TRUE)
  q_975 <- apply(samp_summ, 1, quantile, probs = c(.975), na.rm = TRUE)
  q_sd <- apply(samp_summ, 1, sd, na.rm = TRUE)
  
  cty_star_df <- samp_df2 %>%
    select(-contains("V")) %>%    
    dplyr::mutate(q_5 = q_5, q_025 = q_025, q_975 = q_975, q_sd = q_sd) %>%
    dplyr::filter(fips_st != "11")

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


#### calculate new cty bias ################################
compare_true_star_ctyDat <- function(trueDat, starDat){

  true_cl <- trueDat %>%
    dplyr::select(season, fips, obs_y) %>%
    dplyr::mutate(true_y = obs_y-1) 
  star_cl <- starDat %>%
    dplyr::select(season, fips, db_obs, q_5, q_025, q_975, q_sd) %>%
    dplyr::rename(true_y_st = db_obs)
  fullDat <- full_join(true_cl, star_cl, by = c("season", "fips")) %>%
    dplyr::mutate(std_resid = (true_y-q_5)/q_sd) %>%
    dplyr::mutate(match = ifelse(true_y >= q_025 & true_y <= q_975, TRUE, FALSE))

  return(fullDat)
}
