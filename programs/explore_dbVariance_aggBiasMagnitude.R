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

fnames <- list.files()
uqCombos <- gsub(".csv", "", gsub("aggBias_", "", grep("aggBias", fnames, value = TRUE)))
ctyCombos <- grep("cty", uqCombos, value = TRUE)
stCombos <- grep("st", uqCombos, value = TRUE)

################################
import_datasets <- function(combo){
  print(match.call())

  biasDat <- read_csv(paste0("aggBias_", combo, ".csv"), col_types = "cdd")
  varDat <- read_csv(paste0("dbVariance_", combo, ".csv"), col_types = "cdiid")
  scale <- unlist(strsplit(combo, "_"))[1]
  measure <- unlist(strsplit(combo, "_"))[2]
  print(measure)
  fullDat <- full_join(biasDat, varDat) %>%
    mutate(combo = combo) %>%
    mutate(scale = scale, measure = measure)

  return(fullDat)
}

#### MAIN ################################
ctyDat <- map_df(ctyCombos, import_datasets) # bound dataframes
stDat <- map_df(stCombos, import_datasets)
uqMeasures <- gsub("cty_", "", ctyCombos)

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
