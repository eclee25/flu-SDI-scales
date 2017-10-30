## Name: Elizabeth Lee
## Date: 9/27/17
## Function: examine whether increasing variance in disease burden is correlated with increasing aggregation bias -- at state pooling (across county and seasons) and county pooling levels (across seasons only)
## Filenames: 
## Data Source: 
## Notes: 
################################
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export/test_dbVariance_aggBiasMagnitude")
################################

fnames <- list.files(pattern = "irDt")
uqCombos <- gsub(".csv", "", gsub("aggBias_", "", grep("aggBias", fnames, value = TRUE)))
ctyCombos <- grep("cty", uqCombos, value = TRUE)
stCombos <- grep("st", uqCombos, value = TRUE)

################################
import_datasets <- function(combo){
  print(match.call())

  biasDat <- read_csv(paste0("aggBias_", combo, ".csv"), col_types = "cdd")
  varDat <- read_csv(paste0("dbVariance_", combo, ".csv"))
  scale <- unlist(strsplit(combo, "_"))[1]
  measure <- unlist(strsplit(combo, "_"))[3] # 3 for irDt measures, 2 for non-irDt measures
  print(measure)
  fullDat <- full_join(biasDat, varDat) %>%
    mutate(combo = combo) %>%
    mutate(scale = scale, measure = measure)

  return(fullDat)
}

#### MAIN ################################
#### statistics ##################################
ctyDat <- map_df(ctyCombos, import_datasets) # bound dataframes
stDat <- map_df(stCombos, import_datasets)
uqMeasures <- gsub("cty_irDt_", "", ctyCombos)

ctyTestResults <- purrr::map(uqMeasures, function(x){
  measureDat <- ctyDat %>% filter(measure == x)
  testresults <- cor.test(measureDat$obs_aggBiasMag, measureDat$variance, paired = TRUE)
  return(testresults)
}) # All measures but wksToEpi are correlated. Magnitude measures have much stronger correlations than timing ones.

stTestResults <- purrr::map(uqMeasures, function(x){
  measureDat <- stDat %>% filter(measure == x)
  testresults <- cor.test(measureDat$obs_aggBiasMag, measureDat$variance, paired = TRUE)
  return(testresults)
}) # All measures but wksToEpi are correlated. Magnitude measures have much stronger correlations than timing ones.

#### clean data ##################################
prep_plotDatSt <- purrr::map(uqMeasures, function(x){
  measureDat <- stDat %>% 
    filter(measure == x) %>%
    filter(!is.na(st))
  pltLabels <- measureDat %>% arrange(hetRank) %>% select(hetRank, st)
  measureDat2 <- measureDat %>%
    mutate(xplot = factor(hetRank, levels = pltLabels$hetRank, labels = pltLabels$st))
  return(measureDat2)
})

#### plot data ##################################

plot_st_function <- function(plotDat){
  plotSt <- ggplot(plotDat, aes(x = xplot, y = obs_aggBiasMag)) +
    geom_point() + 
    scale_x_discrete(paste("State, ranked by variance in", plotDat$measure[1])) +
    scale_y_continuous("Magnitude in Aggregation Bias") +
    theme_bw() +
    theme(axis.text =element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  return(plotSt)
} 

stPlots <- lapply(prep_plotDatSt, plot_st_function)
# There doesn't seem to be a relationship between variance in disease burden and aggregation bias or aggreation bias magnitude
# 10/30/17