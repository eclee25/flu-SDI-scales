
## Name: Elizabeth Lee
## Date: 8/16/17
## Function: main code to perform quantitative comparisons of models
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaModelComparisons.R")
source("source_clean_response_functions_cty.R")

################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_st = path_response_st,
                  path_response_cty = path_response_cty)

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

## PLOTS ##
##############################################################################
### FIT CHOROS - Compare 2 fitted value distributions ###################
pairLs <- c("8f_wksToEpi_v2-8", "8f_wksToEpi_rmFixed")
abbrLs <- gsub("_wksToEpi_v", "V", pairLs)
pair_plotFormats <- list(w = 10, h = 5, descrip = paste(abbrLs[1], abbrLs[2], sep = "_"), lvls = pairLs, labs = abbrLs)
pair_datFormats <- list(fit_dataType = "posteriorSamples", refModCode = "8f_wksToEpi_v2-8") # fit_dataType = "summaryStats", "posteriorSamples"
choro_pairFitCompare_overlap(pairLs, pair_plotFormats, pair_datFormats)

# 
# ### OBSFIT CHOROS - Compare point and fitted value distribution for single modCode ###################
# modCodeLs <- c("8f_wksToEpi_v2-8", "10f_wksToEpi_v1-6")
# abbrLs <- gsub("_wksToEpi_v", "V", modCodeLs)
# obsfit_plotFormats <- list(w = 10, h = 5, descrip = abbrLs)
# obsfit_datFormats <- list(fit_dataType = "posteriorSamples", refModCode = "8f_wksToEpi_v2-8") # fit_dataType = "summaryStats", "posteriorSamples"
# choro_obsFitCompare_overlap(modCodeLs, obsfit_plotFormats, obsfit_datFormats)
