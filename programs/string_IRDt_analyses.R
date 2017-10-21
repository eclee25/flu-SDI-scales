
## Name: Elizabeth Lee
## Date: 10/1/17
## Function: string together analyses for detrended incidence ratio (ir.dt)
## Filenames: 
## Data Source: 
## Notes: 
## 8/16/16 - new downscaling procedure to convert ILIn from zip3 to county level, control flow "write_loess_fits_ILIn_cty.R"
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())


setwd(dirname(sys.frame(1)$ofile))
source("explore_loess_fits_IR.R")
source("write_periodicReg_fits_irDt_Octfit.R")
source("write_fullIndic_periodicReg_irDt.R")
source("explore_periodicReg_fits_irDt.R")
source("write_relativeDiseaseBurden_irDt.R")
source("explore_dbMetricsDistribution_irDt.R")
source("explore_periodicReg_inSeasonFits_irDt.R")

# zip to county conversion functions
source("source_clean_response_functions_cty.R")

# run outside of string, after "write_periodicReg_fits_ilinDt_Octfit.R"
# determines the lower threshold for consecutive weeks above the epidemic threshold
# source("explore_fluSeasonDefinition_ilinDt.R") 

#### custom functions ####################################
scaleRename <- function(sp.scale, dataset){
  if (sp.scale == 'zip3'){
    dataset2 <- dataset %>% rename(zip3 = scale)
  } else if (sp.scale == 'state'){
    dataset2 <- dataset %>% rename(state = scale)
  } else if (sp.scale == 'county'){
    dataset2 <- dataset %>% rename(fips = scale)
  } else if (sp.scale == 'region'){
    dataset2 <- dataset %>% rename(region = scale)
  } else if (sp.scale == 'national'){
    dataset2 <- dataset %>% rename(national = scale)
  }
  return(dataset2)
}
###################
# return maximum number of consecutive T values in x
rle.func <- function(x){
  rle.results = rle(x)
  return(max(0, rle.results$lengths[which(rle.results$values)]))
}
###################
substr.Right <- function(x, numchar){
  return(substr(x, nchar(x)-(numchar-1), nchar(x)))
}


#### set these! ####################################
spatial.scale <- "county"
agegroups <- "_totAge" # _totAge, _child, _adult
span.list <- seq(0.4, 0.42, by=0.1)
deg <- 2

#### control flow for spatial scale ####################################
spatial.params <- list()
if (spatial.scale == "state"){
  spatial.params <- list(scale = spatial.scale, stringcode = "State", stringabbr = "_st", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "")
  source("write_loess_fits_IR.R")
} else if (spatial.scale == "zip3"){
  spatial.params <- list(scale = spatial.scale, stringcode = "Zip3", stringabbr = "", serv = "_totServ", servToggle = "") 
  source("write_loess_fits_IR.R")
} else if (spatial.scale == "county" & agegroups == "_totAge"){
  spatial.params <- list(scale = spatial.scale, stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "", age = agegroups, ageToggle = "") 
  source("write_loess_fits_IR_cty.R")
} else if (spatial.scale == "county" & (agegroups %in% c("_child", "_adult"))){
  spatial.params <- list(scale = spatial.scale, stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "", age = agegroups, ageToggle = agegroups) 
  source("write_loess_fits_IR_age_cty.R")
} else if (spatial.scale == "region"){ # added 9/8/17 
  spatial.params <- list(scale = spatial.scale, stringcode = "Region", stringabbr = "_reg", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "")
  source("write_loess_fits_IR.R")
} else if (spatial.scale == "national"){ # added 9/8/17 
  spatial.params <- list(scale = spatial.scale, stringcode = "National", stringabbr = "_nat", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "")
  source("write_loess_fits_IR.R")
  # only through explore_periodicReg_fits_ilinDt
} 

# serv = "_totServ", servToggle = ""
# serv = "_emergency", servToggle = "_emergency"

# age = "_totAge", ageToggle = ""
# age = "_child", ageToggle = "_child" 
# age = "_adult", ageToggle = "_adult"

for (span in span.list){
  params <- list(span.var = span, degree.var = deg, spatial = spatial.params)

  do.call(write_loess_fits_IR, c(params))
  do.call(explore_loess_fits_IR, c(params))
  do.call(write_periodicReg_fits_irDt_Octfit, c(params))
  do.call(write_fullIndic_periodicReg_irDt, c(params))
  do.call(explore_periodicReg_fits_irDt, c(params))
  do.call(write_relativeDiseaseBurden_irDt, c(params))
  do.call(explore_dbMetricsDistribution_irDt, c(params))
  do.call(explore_periodicReg_inSeasonFits_irDt, c(params))
}

