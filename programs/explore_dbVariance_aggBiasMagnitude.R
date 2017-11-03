## Name: Elizabeth Lee
## Date: 9/27/17
## Function: examine whether increasing variance in disease burden is correlated with increasing aggregation bias -- at state pooling (across county and seasons) and county pooling levels (across seasons only)
## Filenames: 
## Data Source: 
## Notes: 
################################
rm(list = ls())
require(tidyverse)
require(broom)
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export/test_dbVariance_aggBiasMagnitude")
#### set these ################################
biasType <- "Diff" # Ratio, Diff
testType <- "pearson"
fnames <- list.files(pattern = biasType) 
uqCombos <- gsub(".csv", "", gsub(sprintf("aggBias%s_yVariance_st_", biasType), "", fnames))
uqMeasures <- gsub("irDt_", "", uqCombos)

#### FUNCTIONS ################################
import_datasets <- function(combo){
  print(match.call())

  inDat <- read_csv(paste0("aggBias", biasType, "_yVariance_st_", combo, ".csv"), col_types = "dcdd")
  
  measure <- unlist(strsplit(combo, "_"))[2] 
  fullDat <- inDat %>%
    mutate(combo = combo) %>%
    mutate(measure = measure)

  return(fullDat)
}
################################
test_aggBiasMag_yVar <- function(measureDat){
  seasLs <- measureDat %>% distinct(season) %>% unlist
  testresultsDf <- data.frame()
  for (s in seasLs){
    seasDat <- measureDat %>% filter(season == s)
    testresults <- tidy(cor.test(seasDat$obs_aggBiasMag, seasDat$obs_yVariance, method = testType)) %>%
      mutate(season = s) %>% 
      mutate(measure = measureDat$measure[1]) 
    
    testresultsDf <- bind_rows(testresultsDf, testresults)
  } 
  return(testresultsDf)
}
################################
plot_st_function <- function(plotDat){
  plotSt <- ggplot(plotDat, aes(x = obs_yVariance, y = obs_aggBiasMag)) +
    geom_point() + 
    scale_x_continuous(paste("Within-State Variance in", plotDat$measure[1])) +
    scale_y_continuous("State-County Aggregation Bias Magnitude") +
    theme_bw() +
    theme(axis.text =element_text(size = 10)) +
    facet_wrap(~season, scales = "free")
  
  ggsave(paste0("../../graph_outputs/explore_dbVariance_aggBiasMagnitude/explore_dbVariance_aggBiasMagnitude_", biasType, "_", plotDat$measure[1], ".png"), plotSt, dpi = 300, width = 6, height = 4)
  return(plotSt)
} 

#### MAIN ################################
#### import data ##################################
stDat <- map_df(uqCombos, import_datasets)
#### clean data ##################################
fullDat <- stDat %>%
  filter(!is.na(obs_yVariance)) %>%
  mutate(obs_aggBiasMag = abs(obs_aggBias))
#### statistics ##################################
testResults <- fullDat %>%
  split(.$measure) %>%
  map_df(~test_aggBiasMag_yVar(.)) # 
write_csv(testResults, paste0("testOutputs/cor_dbVariance_aggBiasMagnitude", biasType, "_", testType, ".csv"))

#### plot data ##################################
stPlots <- fullDat %>%
  split(.$measure) %>%
  purrr::map(~plot_st_function(.))
# 11/3/17