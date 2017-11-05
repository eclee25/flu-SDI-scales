require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))


#### path functions ################################
string_coef_fname <- function(modCodeStr){
  return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/summaryStats_", modCodeStr, ".csv"))
}
################################
string_modData_fname <- function(modCodeStr){
  splitmod <- unlist(strsplit(modCodeStr, "_"))
  return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_import/inlaImport_model", splitmod[1], "_", splitmod[2], "_irDt_v7.csv"))
}


#### linear model components ################################
################################
grab_coef_directionality <- function(modCodeStr, coefName){
  print(match.call())
  # grab coefficient directionality for the correction factor of interest

  coefDat <- read_csv(string_coef_fname(modCodeStr), col_types = cols_only(RV = "c", mean = "d"))
  coefDirection <- coefDat %>%
    filter(RV == paste0(coefName, "_nonzero")) %>%
    mutate(direction = ifelse(mean > 0 , 1, -1)) %>%
    select(direction) %>%
    unlist

  return(coefDirection)
}
################################
grab_predictor_data <- function(modCodeStr, coefName){
  print(match.call())
  # grab predictor data information

  predData <- read_csv(string_modData_fname(modCodeStr)) %>%
    select_("fips", "fips_st", "season", coefName) %>%
    rename_("corrFactor" = coefName)

  return(predData)
}
################################
create_proxy_data <- function(modCodeStr, coefName, trueStObsDat){
  print(match.call())

  # create new dataframe and classifer (import trueStObsDat directly in main with source_import_modeldata.R)
  predDat <- grab_predictor_data(modCodeStr, coefName)
  coefDirection <- grab_coef_directionality(modCodeStr, coefName)
  fullDat <- left_join(trueStObsDat, predDat, by = c("fips_st", "season")) %>%
    rename(stBurden = obs_y) %>%
    mutate(proxyValue = stBurden + (coefDirection*corrFactor)) 

  return(fullDat)
}
################################
create_classifier_data_cty <- function(fullDat){
    print(match.call())

  ctyQuants <- seq(0, 1, by = 0.1)
  classiferDatCty <- map_df(ctyQuants, function(x){

    threshold <- quantile(fullDat$proxyValue, probs = x, na.rm = TRUE)

    newDat <- fullDat %>%
      mutate(quantLvl = x) %>%
      mutate(thresholdCty_proxy = threshold) %>%
      mutate(testClassif = ifelse(proxyValue > thresholdCty_proxy, TRUE, FALSE)) %>%
      select(fips, season, proxyValue, quantLvl, thresholdCty_proxy, testClassif)

    return(newDat)
    })

  return(classiferDatCty)
}
################################
create_classifier_data_st <- function(fullDat){
    print(match.call())

  stQuants <- seq(0, 1, by = 0.25)
  classiferDatSt <- map_df(stQuants, function(x){

    thresholdDf <- fullDat %>%
      group_by(fips_st) %>%
      summarise(thresholdSt_proxy = quantile(proxyValue, probs = x, na.rm = TRUE)) %>%
      select(fips_st, thresholdSt_proxy)

    newDat <- fullDat %>%
      mutate(quantLvl = x) %>%
      left_join(thresholdDf, by = c("fips_st")) %>%
      mutate(testClassif = ifelse(proxyValue > thresholdSt_proxy, TRUE, FALSE)) %>%
      select(fips, season, proxyValue, quantLvl, thresholdSt_proxy, testClassif)

    return(newDat)
    })

  return(classiferDatSt)
}


#### true classifications ################################
################################
apply_true_classifications_cty <- function(trueCtyObsDat){
  print(match.call())

  trueCtyObsDat2 <- trueCtyObsDat %>%
    mutate(fips_st = substring(fips, 1, 2)) %>%
    rename(ctyBurden = obs_y) 

  ctyQuants <- seq(0, 1, by = 0.1)
  trueCtyClassif <- map_df(ctyQuants, function(x){
    
    threshold <- quantile(trueCtyObsDat2$ctyBurden, probs = x, na.rm = TRUE)
    
    newDat <- trueCtyObsDat2 %>% 
      mutate(quantLvl = x) %>%
      mutate(thresholdCty_true = threshold) %>%
      mutate(trueClassif = ifelse(ctyBurden > thresholdCty_true, TRUE, FALSE))
    
    return(newDat)
    })

  return(trueCtyClassif)
}
################################
apply_true_classifications_st <- function(trueCtyObsDat){
  print(match.call())

  trueCtyObsDat2 <- trueCtyObsDat %>%
    mutate(fips_st = substring(fips, 1, 2)) %>%
    rename(ctyBurden = obs_y) 

  stQuants <- seq(0, 1, by = 0.25)
  trueStClassif <- map_df(stQuants, function(x){

    thresholdDf <- trueCtyObsDat2 %>%
      group_by(fips_st) %>%
      summarise(thresholdSt_true = quantile(ctyBurden, probs = x, na.rm = TRUE)) %>%
      select(fips_st, thresholdSt_true)
    
    newDat <- trueCtyObsDat2 %>%
      mutate(quantLvl = x) %>%
      left_join(thresholdDf, by = c("fips_st")) %>%
      mutate(trueClassif = ifelse(ctyBurden > thresholdSt_true, TRUE, FALSE))
    
    return(newDat)
    })

  return(trueStClassif)  
}
################################

#### identify sensitive (true positive) and specific (true negative) values ################################
identify_sensitive_specific <- function(fullDat){
  print(match.call())

  fullDat2 <- fullDat %>%
    select(season, fips, quantLvl, trueClassif, testClassif) %>%
    filter(!is.na(trueClassif) & !is.na(testClassif)) %>%
    mutate(sensitive = ifelse(trueClassif & testClassif, 1, 0)) %>%
    mutate(specific = ifelse(!trueClassif & !testClassif, 1, 0))
  
  plotDat <- fullDat2 %>%
    group_by(quantLvl) %>%
    summarise(sensitive = sum(sensitive), specific = sum(specific), n = n()) %>%
    mutate(sensitivity = sensitive/n) %>%
    mutate(specificity = specific/n) %>%
    mutate(falsePR = 1-specificity)

  return(plotDat)
}
