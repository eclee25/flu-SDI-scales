
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: string together analyses for detrended ili metric divided by population size (ilin.dt)
## Filenames: 
## Data Source: 
## Notes: 12/12/15 - add switch between state-level and zip3-level
## 8/16/16 - new downscaling procedure to convert ILIn from zip3 to county level, control flow "write_loess_fits_ILIn_cty.R"
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())


setwd(dirname(sys.frame(1)$ofile))
source("explore_loess_fits_ILIn.R")
source("write_periodicReg_fits_ilinDt_Octfit.R")
source("write_periodicReg_fits_ilinDt_Octfit_emergency.R")
source("write_fullIndic_periodicReg_ilinDt.R")
source("explore_periodicReg_fits_ilinDt.R")
source("write_relativeDiseaseBurden_ilinDt.R")
source("explore_dbMetricsDistribution_ilinDt.R")
source("explore_periodicReg_inSeasonFits_ilinDt.R")

# zip to county conversion functions
source("source_clean_response_functions_cty.R")

# run outside of string, after "write_periodicReg_fits_ilinDt_Octfit.R"
# determines the lower threshold for consecutive weeks above the epidemic threshold
# source("explore_fluSeasonDefinition_ilinDt.R") 

#### set these! ####################################
spatial.scale <- "state"
agegroups <- "_totAge" # _totAge, _child, _adult
span.list <- seq(0.4, 0.42, by=0.1)
deg <- 2

#### control flow for spatial scale ####################################
spatial.params <- list()
if (spatial.scale == "state"){
  spatial.params <- list(scale = spatial.scale, stringcode = "State", stringabbr = "_st", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "")
  source("write_loess_fits_ILIn.R")
} else if (spatial.scale == "zip3"){
  spatial.params <- list(scale = spatial.scale, stringcode = "Zip3", stringabbr = "", serv = "_totServ", servToggle = "") 
  source("write_loess_fits_ILIn.R")
} else if (spatial.scale == "county" & agegroups == "_totAge"){
  spatial.params <- list(scale = spatial.scale, stringcode = "County", stringabbr = "_cty", serv = "_emergency", servToggle = "_emergency", age = agegroups, ageToggle = "") 
  source("write_loess_fits_ILIn_cty.R")
} else if (spatial.scale == "county" & (agegroups %in% c("_child", "_adult"))){
  spatial.params <- list(scale = spatial.scale, stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "", age = agegroups, ageToggle = agegroups) 
  source("write_loess_fits_ILIn_age_cty.R")
}  

# serv = "_totServ", servToggle = ""
# serv = "_emergency", servToggle = "_emergency"

# age = "_totAge", ageToggle = ""
# age = "_child", ageToggle = "_child" 
# age = "_adult", ageToggle = "_adult"

for (span in span.list){
  params <- list(span.var = span, degree.var = deg, spatial = spatial.params)

  # do.call(write_loess_fits_ILIn, c(params))
  # do.call(explore_loess_fits_ILIn, c(params))
  # do.call(write_periodicReg_fits_ilinDt_Octfit, c(params))
  # do.call(write_fullIndic_periodicReg_ilinDt, c(params))
  # do.call(explore_periodicReg_fits_ilinDt, c(params))
  # do.call(write_relativeDiseaseBurden_ilinDt, c(params))
  do.call(explore_dbMetricsDistribution_ilinDt, c(params))
  do.call(explore_periodicReg_inSeasonFits_ilinDt, c(params))
}

