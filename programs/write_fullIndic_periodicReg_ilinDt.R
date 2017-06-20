
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: write full datasets from periodic regression data of ilin.dt, in preparation for write_relativeDiseaseBurden_ILIn.R: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILIn.R, Analysis 3 for additional details)

## Filenames: periodicReg_%sall%sMods_ilinDt_Octfit%s.csv
## Data Source: IMS Health 
## Notes: 12/15/15 - Refer to explore_fluSeasonDefinition_ilinDt.R for definition of "flu epidemic". States are considered to have experienced a flu epidemic if they had 5+ consecutive weeks above the epidemic threshold in the flu period. 
## 10-20-15 - Split program with write_relativeDiseaseBurden_ILI.R; data export files used in write_relativeDiseaseBurden_ILI.R
## 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
# 
write_fullIndic_periodicReg_ilinDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  require(ISOweek)
  require(tidyr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### local functions ####################################
  # return logical column; T if data should be considered as "flu season" (ie. data falls within period with maximum consecutive weeks)
  consider.flu.season <- function(x){
    rle.results = rle(x)
    max.consec = max(0, rle.results$lengths[which(rle.results$values)]) # which(rle.results$values) works because values are boolean
    dummy <- rep(F, length(x))
    pre.index <- (which(rle.results$values & rle.results$lengths==max.consec))[1] # rle index of first value in longest consecutive run of TRUE
    post.index <- tail((which(rle.results$values & rle.results$lengths==max.consec)), n=1) # rle index of last value in longest consec run of TRUE
    converted.pre.index <- ifelse(pre.index==1, 0, sum(rle.results$lengths[1:(pre.index-1)])) # vector index - 1 of first value in longest consec run of TRUE
    #   converted.post.index <- converted.pre.index + max.consec
    #   print(rle.results)
    #   print(pre.index)
    #   print(which(rle.results$values & rle.results$lengths==max.consec))
    converted.post.index <- ifelse(post.index, sum(rle.results$lengths[1:post.index]), NA) # vector index of last value in longest consec run of TRUE
    if(!is.na(converted.pre.index)){
      dummy[(converted.pre.index+1):converted.post.index] <- T
    }
    return(dummy) # full vector of T/F
  }
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit"
  
#   # uncomment when running script separately
#   spatial <- list(scale = "zip3", stringcode = "Zip3", stringabbr = "", serv = "_emergency", servToggle = "_emergency", age = "_totAge", ageToggle = "")
#   span.var <- 0.4 # 0.4, 0.6
#   degree.var <- 2
  
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### data processing (based on explore_fluSeasonDefinition_ILI.R) ####################################
  setwd('../R_export')
  
  if (spatial$scale == 'zip3'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, code.str), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = zip3)
    num.weeks <- 5 # see explore_fluSeasonDefinition_ilinDt.R
    } else if (spatial$scale == 'state'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s.csv', code, spatial$stringcode, code2, code.str), col_types=list(state = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = state)
    num.weeks <- 2 # see explore_fluSeasonDefinition_ilinDt.R
  } else if (spatial$scale == 'county'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, spatial$ageToggle, code.str), col_types=list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = fips)
    num.weeks <- 2 # 8/19/16 reduced because incl.analysis excludes counties that are too noisy anyways # see explore_fluSeasonDefinition_ilinDt.R
  }
  
  # 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
  data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) 
  
  # 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
  # 8/20/16 if ILIn >= .fitted (incl.lm2), epi.thresh = 0 and epi.week = FALSE
  data3 <- data2 %>% filter(incl.lm) %>% mutate(epi.thresh = ifelse(incl.lm2, .fitted+(1.96*.se.fit), 0)) %>% mutate(epi.week = ilin.dt>epi.thresh)
  
  ## See explore_fluSeasonDefinition_IR.R for derivation of flu season definition
  # 9/15/15: filter out zip3-season combinations with equivalent or more ILI activity in the previous non-flu season than flu season (season 1 will use subsequent non-flu season)
  dummy.flu <- data3 %>% filter(flu.week) %>% group_by(season, scale) %>% summarise(consec.flu.epiweeks = rle.func(epi.week))
  # 6/3/16: include only May-Sept immediately preceding flu season
  dummy.nf <- data3 %>% filter(!flu.week & month < 10) %>% group_by(season, scale) %>% summarise(consec.nf.epiweeks = rle.func(epi.week)) %>% ungroup %>% mutate(season=season+1)
  # 9/15/15 for season 1, use season 1 consec.nf.epiweeks, which occur after the season 1 flu period
  dummy.nf2 <- bind_rows((dummy.nf %>% filter(season==2) %>% ungroup %>% mutate(season=1)), dummy.nf)
  # summarize season-zip3 combinations that have epidemics (num.weeks+ consecutive epidemic weeks)
  # 8/20/16: incl.analysis = consec.flu.epiweeks >= consec.nf.epiweeks
  zip3s_with_epi <- full_join(dummy.flu, dummy.nf2, by=c("season", "scale")) %>% mutate(incl.analysis = consec.flu.epiweeks >= consec.nf.epiweeks) %>% mutate(has.epi = (consec.flu.epiweeks>=num.weeks))
  
  # join summary data to full dataset (adds has.epi and incl.analysis indicators)
  data4 <- right_join(data3, zip3s_with_epi %>% select(-contains("consec.")), by=c("season", "scale"))
  # 9/15/15: in.season indicator: must meet flu.week, has.epi, incl.analysis, and consecutive epi.week criteria (FLU PERIOD DATA ONLY)
  data5 <- data4 %>% filter(flu.week & has.epi & incl.analysis) %>% group_by(season, scale) %>% mutate(in.season = consider.flu.season(epi.week))
  data6 <- left_join(data4, (data5 %>% ungroup %>% select(Thu.week, scale, in.season)), by = c("Thu.week", "scale")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) # rm filter(incl.analysis)
  
  # rename variable "scale" to zip3 or state
  data5_write <- scaleRename(spatial$scale, data5)
  data6_write <- scaleRename(spatial$scale, data6)
  
  # save to file 
  print(sprintf('writing full indicators to file %s', code.str))
  # these data are used in "write_relativeDiseaseBurden_ilinDt.R" for further processing of disease burden metrics
  write.csv(data5_write, file = sprintf('fullIndicFlu_periodicReg_%silinDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), row.names=FALSE)
  write.csv(data6_write, file = sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), row.names=FALSE)
  
}



