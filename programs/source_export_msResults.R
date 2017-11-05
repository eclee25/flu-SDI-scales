require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
################################################################

#### filepath functions ################################
string_msResults_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/msResults/"))
}
################################

#### reporting functions ################################
################################
identify_aggBias_hotSpots <- function(datFormats){
  print(match.call())

  # data formatting
  dbCode <- datFormats$dbCode; scaleDiff <- datFormats$scaleDiff
  neighSize <- datFormats$neighSize
  quant_aggBias_threshold <- datFormats$quant_aggBias_threshold # 0.99 for top 1% aggBias magnitude

  # import data
  seasons <- 3:9
  lisaDat <- map_df(seasons, function(s){
    lisaDat_seas <- read_csv(paste0(string_exportDat_aggBias_data_folder(), "lisa_aggBias_", scaleDiff, "_", dbCode, "_neighSize", neighSize, "_S", s, ".csv")) %>%
      mutate(obs_aggBiasMag = abs(obs_aggBiasMag))
    return(lisaDat_seas)
    })

  # identify lisa hotspots with a large magnitude of aggregation bias
  thresh <- quantile(lisaDat$obs_aggBiasMag, probs = quant_aggBias_threshold)
  hotspotDat <- lisaDat %>%
    group_by(season) %>%
    filter(mean > mean(mean, na.rm = TRUE)) %>%
    ungroup %>%
    filter(obs_aggBiasMag > thresh)

  write_csv(hotspotDat, paste0(string_msResults_folder(), "aggBias_hotSpots_", scaleDiff, "_", dbCode, "_neighSize", neighSize, ".csv"))

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
    neighSize = 160)) %>%
    mutate(pltVar = paste0("obs_diff_", scaleDiff))
hotspotLs = list()
for (i in 1:nrow(hotspotFormatsDf)){
  hotspotLs[[i]] <- identify_aggBias_hotSpots(as.list(hotspotFormatsDf[i,]))
}
