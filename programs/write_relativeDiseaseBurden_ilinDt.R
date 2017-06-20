
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: write relative magnitude of disease burden with detrended ILIn (ilin.dt) Octfit data; data processing: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILIn.R, Analysis 3 for additional details)
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 

## Filenames: periodicReg_%sallZip3Mods_ilinDt_Oct.csv
## Data Source: IMS Health 
## Notes: 12/15/15 - Refer to explore_fluSeasonDefinition_ILIn.R for definition of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 5+ consecutive weeks above the epidemic threshold in the flu period. 
## 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_relativeDiseaseBurden_ilinDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  require(ISOweek)
  require(tidyr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit"
  
#   # uncomment when running script separately
#   spatial <- list(scale = "zip3", stringcode = "Zip3", stringabbr = "", serv = "_emergency", servToggle = "_emergency")
#   span.var <- 0.4 # 0.4, 0.6
#   degree.var <- 2
  
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### read saved data from write_fullIndic_periodicReg_ilinDt.R ##################
  
  if (spatial$scale == 'zip3'){
    setwd('../R_export')
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, code.str, spatial$stringabbr), col_names = T, col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double(), in.season = col_logical(), has.epi = col_logical(), incl.analysis = col_logical())) %>%
      rename(scale = zip3)
    
    # import zip3 lat/long coordinate data
    setwd('../reference_data')
    import <- read_csv(file='Coord3digits.csv')
    coordsData <- tbl_df(import) %>% select(zip3, STATE, st_FIPS, pop, lat, long, w_lat, w_long) %>% 
      mutate(zip3 = substr.Right(paste0("00", zip3), 3)) %>% 
      select(-w_lat, -w_long, -pop) %>%
      rename(scale = zip3)
    
  } else if (spatial$scale == 'state'){
    setwd('../R_export')
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s_analyzeDB%s.csv', code, code2, code.str, spatial$stringabbr), col_names = T, col_types = list(state = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double(), in.season = col_logical(), has.epi = col_logical(), incl.analysis = col_logical())) %>%
      rename(scale = state)
    
    # import state lat/long coordinate data
    setwd('../reference_data')
    coordsData <- read_csv(file='state_latlon.csv') %>%
      rename(scale = state, lat = latitude, long = longitude)
    
  } else if (spatial$scale == 'county'){
    setwd('../R_export')
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), col_names = T, col_types = list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double(), in.season = col_logical(), has.epi = col_logical(), incl.analysis = col_logical())) %>%
      rename(scale = fips)
    
    # import state lat/long coordinate data
    setwd('../reference_data')
    coordsData <- read_csv(file='cty_pop_latlon.csv') %>%
      rename(scale = fips, lat = latitude, long = longitude)
  }
  
  
  #### create dz burden metrics ##################
  # 7/18/16 change !incl.analysis db outputs from magnitude = 0 to magnitude = NA (data were too noisy to calculate disease burden)
  # 6/3/16 include incl.analysis = F in db outputs, magnitude = 0 if incl.analysis = F (instead of excluded from fullIndicAll exported, as before)
  # 5/6/16 include has.epi = F in db outputs, magnitude = 0 if no epi (instead of NA as before)
  # 7/27/15 update dz burden metrics (\cite{Viboud2014} for inspiration of 1b & 1c)
  # create disease burden metrics: 1a) sum ILI across epidemic weeks (overall magnitude), 1b) cumulative difference in ILI and baseline (second proxy of overall magnitude), 1c) cumulative difference in ILI and epidemic threshold (third proxy of overall magnitude), 2) rate of ILI at epidemic peak, 3) epidemic duration
  dbMetrics.epi <- data5 %>% filter(has.epi & incl.analysis) %>% group_by(season, scale) %>% filter(in.season) %>% summarize(has.epi = first(has.epi), incl.analysis = first(incl.analysis), ilinDt.sum = sum(ilin.dt, na.rm=T), ilinDt.excess.BL = sum(ilin.dt-.fitted, na.rm=T), ilinDt.excess.thresh = sum(ilin.dt-epi.thresh, na.rm=T), ilinDt.peak = max(ilin.dt, na.rm=T), epi.dur = sum(in.season))
  dbMetrics.noepi <- data5 %>% filter(!has.epi & incl.analysis) %>% mutate(ilinDt.modified = 0) %>% group_by(season, scale) %>% summarize(has.epi = first(has.epi), incl.analysis = first(incl.analysis), ilinDt.sum = sum(ilinDt.modified, na.rm=T), ilinDt.excess.BL = sum(ilinDt.modified, na.rm=T), ilinDt.excess.thresh = sum(ilinDt.modified, na.rm=T), ilinDt.peak = max(ilinDt.modified, na.rm=T), epi.dur = sum(ilinDt.modified, na.rm=T))
  dbMetrics.noisy <- data5 %>% filter(!incl.analysis) %>% mutate(ilinDt.modified = NA) %>% group_by(season, scale) %>% summarize(has.epi = first(has.epi), incl.analysis = first(incl.analysis), ilinDt.sum = first(ilinDt.modified), ilinDt.excess.BL = first(ilinDt.modified), ilinDt.excess.thresh = first(ilinDt.modified), ilinDt.peak = first(ilinDt.modified), epi.dur = first(ilinDt.modified))
  
  # 7/18/16 merge dbMetrics
  dbMetrics <- bind_rows(dbMetrics.epi, dbMetrics.noepi, dbMetrics.noisy) 
  
  # 8/6/15 create epidemic start timing and peak timing metrics
  # data processing in preparation for timing metrics
  data7 <- data5 %>% select(Thu.week, t, ilin.dt, season, scale, flu.week, in.season)
  createTiming1 <- data7 %>% group_by(season, scale) %>% mutate(t.minweek = ifelse(Thu.week==min(Thu.week), t, 0)) %>% select(Thu.week, season, scale, t.minweek) %>% ungroup
  createTiming2 <- data7 %>% mutate(ilin.dt = as.numeric(ilin.dt)) %>% group_by(season, scale) %>% 
    filter(in.season) %>% 
    mutate(t.firstepiweek = ifelse(Thu.week==min(Thu.week, na.rm=T), t, 0)) %>% 
    mutate(t.peakweek = as.numeric(ifelse(ilin.dt==max(ilin.dt, na.rm=T), t, 0))) %>% ungroup %>% 
    select(Thu.week, scale, t.firstepiweek, t.peakweek) # as.numeric wrapper on if/else statement for t.peakweek mutate is due to issue: https://github.com/hadley/dplyr/issues/1036
  createTiming <- left_join(createTiming1, createTiming2, by=c("Thu.week", "scale")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))
  
  # create dataset with metrics
  # 4) wks.to.epi = # weeks between start of flu period and start of epidemic; 5) wks.to.peak = # weeks between start of epidemic and peak IR week
  dbMetrics.timing <- createTiming %>% group_by(season, scale) %>% 
    summarise(minweek = max(t.minweek), firstepiweek = max(t.firstepiweek, na.rm=T), peakweek = max(t.peakweek, na.rm=T)) %>% 
    mutate(wks.to.epi = 1 + firstepiweek - minweek) %>% 
    mutate(wks.to.peak = 1 + peakweek - firstepiweek) 
  # merge timing metrics with other dz burden metrics
  dbMetrics2 <- full_join(dbMetrics, dbMetrics.timing, by=c("season", "scale")) %>% 
    select(-minweek, -firstepiweek, -peakweek)
  
  dbMetrics.g <- gather(dbMetrics2, metric, burden, 5:11) 
  # mean and sd for each metric by season for viewing
  dbMetrics_summary <- dbMetrics.g %>% group_by(season, metric) %>% 
    summarize(metric.mn = mean(burden), metric.sd = sd(burden))
  
    #### merge coords with dz burden metrics ##################
  dbMetrics.coords <- left_join(dbMetrics.g, coordsData, by = "scale")
  
  #### save summary data ##################
  print(sprintf('writing db metrics to file %s', code.str))
  
  # rename variable "scale" to zip3 or state
  dbMetrics.g2 <- scaleRename(spatial$scale, dbMetrics.g)
  dbMetrics.coords2 <- scaleRename(spatial$scale, dbMetrics.coords)
  
  # save summary data to file 
  # these data are used in "explore_dbMetricsDistribution_ilinDt.R" for exploratory analysis of outcome metrics
  setwd('../R_export')
  View(dbMetrics.g2 %>% filter(metric=="ilinDt.sum"))
  print(dbMetrics.g2 %>% filter(metric=="ilinDt.sum") %>% group_by(season) %>% count)
  write.csv(dbMetrics.g2, file = sprintf('dbMetrics_periodicReg_%silinDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), row.names=FALSE)
  
  # save summary data to file with coords 
  # these data are used in "analyze_dbMetricsDistance_IR.R" for matrix correlations
  setwd('../R_export')
  write.csv(dbMetrics.coords2, file = sprintf('dbMetrics_periodicReg_%silinDt%s%s%s%s_analyzeDBdist%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), row.names=FALSE)
  
}

