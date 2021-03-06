require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
################################################################

#### filepath functions ################################

#### reporting functions ################################
################################
identify_aggBias_hotSpots_oneSeason <- function(datFormats){
  print(match.call())

  # data formatting
  dbCode <- datFormats$dbCode; scaleDiff <- datFormats$scaleDiff
  neighSize <- datFormats$neighSize
  quant_aggBias_threshold <- datFormats$quant_aggBias_threshold # 0.99 for top 1% aggBias magnitude

  # import data
  seasons <- 3:9
  lisaDat <- map_df(seasons, function(s){
    lisaDat_seas <- read_csv(paste0(string_exportDat_aggBias_data_folder(), "lisa_aggBias_", scaleDiff, "_", dbCode, "_neighSize", neighSize, "_S", s, ".csv"), col_types = "icddddid") %>%
      mutate(obs_aggBiasMag = abs(obs_aggBias))
    return(lisaDat_seas)
    })

  # identify lisa hotspots with a large magnitude of aggregation bias
  biasThresh <- quantile(lisaDat$obs_aggBiasMag, probs = quant_aggBias_threshold, na.rm = TRUE)
  corrThreshDf <- lisaDat %>%
    group_by(season) %>%
    summarise(corrThresh = median(correlation, na.rm = TRUE))

  hotspotDat <- full_join(lisaDat, corrThreshDf, by = c("season")) %>%
    filter(correlation > corrThresh) %>%
    filter(obs_aggBiasMag > biasThresh)

  write_csv(hotspotDat, paste0(string_msResults_folder(), "aggBias_hotSpots_", scaleDiff, "_", dbCode, "_neighSize", neighSize, ".csv"))

  return(hotspotDat)
}
################################
identify_aggBias_hotSpots_allSeasons <- function(datFormats){
  print(match.call())

  # data formatting
  dbCode <- datFormats$dbCode; scaleDiff <- datFormats$scaleDiff
  neighSize <- datFormats$neighSize
  quant_aggBias_threshold <- datFormats$quant_aggBias_threshold # 0.99 for top 1% aggBias magnitude

  # import data
  seasons <- 3:9
  lisaDat_allSeas <- read_csv(paste0(string_exportDat_aggBias_data_folder(), "lisa_aggBias_", scaleDiff, "_", dbCode, "_neighSize", neighSize, "_allSeas.csv"))
  
  avg_aggBias <- lisaDat_allSeas %>% select(contains("z_S"))
  obs_aggBias <- apply(avg_aggBias, 1, mean, na.rm = TRUE)

  lisaDat <- lisaDat_allSeas %>% 
    mutate(obs_aggBias = obs_aggBias) %>%
    mutate(obs_aggBiasMag = abs(obs_aggBias))

  # identify lisa hotspots with a large magnitude of aggregation bias
  biasThresh <- quantile(lisaDat$obs_aggBiasMag, probs = quant_aggBias_threshold, na.rm = TRUE)
  corrThresh <- median(lisaDat$correlation, na.rm = TRUE)

  hotspotDat <- lisaDat %>%
    filter(correlation > corrThresh) %>%
    filter(obs_aggBiasMag > biasThresh)

  write_csv(hotspotDat, paste0(string_msResults_folder(), "aggBias_hotSpots_allSeas_", scaleDiff, "_", dbCode, "_neighSize", neighSize, ".csv"))

  return(hotspotDat)
}
################################

#### MAIN ################################
#### set these ############################
dbCodeStr <- "_irDt_Octfit_span0.4_degree2"

#### identify aggregation bias hotspots ################################
hotspotFormatsDf <- tbl_df(data.frame(
    dbCode = c(rep("irDt_wksToEpi", 2), rep("irDt_wksToPeak", 2), rep("irDt_iliEarly", 2), rep("irDt_iliPeak", 2)),
    scaleDiff = rep(c("stCty", "regCty"), 4),
    neighSize = 160,
    quant_aggBias_threshold = 0.90)) %>%
    mutate(pltVar = paste0("obs_diff_", scaleDiff))

# by season
hotspotLs = list()
for (i in 1:nrow(hotspotFormatsDf)){
  hotspotLs[[i]] <- identify_aggBias_hotSpots_oneSeason(as.list(hotspotFormatsDf[i,]))
}

hotspotFormatsDf2 <- tbl_df(data.frame(
    dbCode = c(rep("irDt_wksToEpi", 2), rep("irDt_wksToPeak", 2), rep("irDt_iliEarly", 2), rep("irDt_iliPeak", 2)), 
    scaleDiff = rep(c("stCty", "regCty"), 4), 
    neighSize = 1000,
    quant_aggBias_threshold = 0.90)) %>%
    mutate(pltVar = paste0("obs_diff_", scaleDiff))

# across all seasons 
hotspotLs2 = list()
for (i in 1:nrow(hotspotFormatsDf2)){
  hotspotLs2[[i]] <- identify_aggBias_hotSpots_allSeasons(as.list(hotspotFormatsDf2[i,]))
}