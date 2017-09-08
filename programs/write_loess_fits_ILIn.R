
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: smooth ILIn data using loess smoother during summer weeks, divide observed ili by smoothed loess fits for entire time series. Pass the processed ILI metric to write_loess_PeriodicReg_....R
## timetrend <- loess(ILIn ~ t) during summer weeks 
## ILInDt <- ILI/pop/timetrend
## lm(ILInDt ~ t + cos(2*pi*t/52.18) + sin(2*pi*t/52.18))

## Filenames: 
## Data Source: 
## Notes: code2 = 'Octfit' --> October to April is flu period, but we fit the seasonal regression from April to October (i.e., expanded definition of summer) in order to improve phase of regression fits
## 52.18 weeks per year in the regression model
## 12-10-15 - add spatial scale option (zip3 or state)
## 8-16-16 - remove county procedure from this program (see write_loess_fits_ILIn_cty.R instead)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_loess_fits_ILIn <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(broom)
  require(tidyr)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))

#   # uncomment when running script separately
#   spatial <- list(scale = "zip3", stringcode = "Zip3", stringabbr = "", serv = "_totServ", servToggle = "")  
#   span.var <- 0.4 # 0.4, 0.6
#   degree.var <- 2
  
  
  #### import data ####################################
  setwd('../R_export')
  if (spatial$scale == 'zip3'){
    ilic_df <- read_csv(sprintf('ilicByall%s_allWeekly%s_totAge.csv', spatial$stringcode, spatial$serv), col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical())) %>%
      rename(scale = zip3)
  } else if (spatial$scale == 'state'){
    ilic_df <- read_csv(sprintf('ilicByall%s_allWeekly_totServ_totAge.csv', spatial$stringcode), col_types = list(state = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical())) %>%
      rename(scale = state)
  } else if (spatial$scale == 'region'){
    ilic_df <- read_csv(sprintf('iliByall%s_allWeekly_totServ_totAge.csv', spatial$stringcode), col_types = list(region = col_character(), ili = col_integer(), pop = col_integer())) %>%
      rename(scale = region)
  } else if (spatial$scale == 'national'){
    ilic_df <- read_csv(sprintf('iliByall%s_allWeekly_totServ_totAge.csv', spatial$stringcode), col_types = list(region = col_character(), ili = col_integer(), pop = col_integer())) %>%
      rename(scale = national)
  }
 
  #### set these! ################################
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### data cleaning ####################################
  # 10/27/15 remove zip3s with missing pop data in incl.lm indicator
  # 7/18/16 incl.lm is redundant with changes in write_ILIc_data.R (incl.lm is already defined as FALSE when is.na(pop)), 100,000 pop multiplier
  ilic_df2 <- ilic_df %>% 
    mutate(incl.lm = ifelse(!incl.lm, FALSE, ifelse(is.na(pop), FALSE, TRUE))) %>% 
    mutate(ILIn = ili/pop*10000)

  # create new data for augment
  newbasedata <- ilic_df2 %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01')) 
  #### perform loess regression ####################################
  allLoessMods <- ilic_df2 %>%
    filter(fit.week) %>% 
    filter(Thu.week < as.Date('2009-05-01')) %>% 
    filter(incl.lm) %>%
    group_by(scale) %>%
    do(fitZip3 = loess(ILIn ~ t, span = span.var, degree = degree.var, data = ., na.action=na.exclude))
    
  allLoessMods_aug <- augment(allLoessMods, fitZip3, newdata= newbasedata)
 
  # after augment - join ILI data to fits
  # 7/18/16 incl.lm2 == .fitted>0
  allLoessMods_fit_ILI <- right_join((allLoessMods_aug %>% ungroup %>% select(-t)), (ilic_df2 %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'scale')) %>% 
    mutate(week=as.Date(week, origin="1970-01-01")) %>% 
    mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% 
    mutate(incl.lm2 = ifelse(.fitted > ILIn, FALSE, TRUE)) %>%
    mutate(ilin.dt = ILIn-.fitted) # 7/28/16: don't make any change to detrending based on incl.lm2
  
  # 7/28/16: new indicator incl.lm2 is false if .fitted > ILIn; true if incl.lm is true
  
  #### write data to file ####################################
  print('writing loess fits')
  setwd('../R_export')
  
  # rename scale variable
  allLoessMods_fit_ILI2 <- scaleRename(spatial$scale, allLoessMods_fit_ILI) 
  
  # write fitted and original loess smoothed ILI data 
  write.csv(allLoessMods_fit_ILI2, file=sprintf('loess%s_all%sMods_ILIn%s.csv', code.str, spatial$stringcode, spatial$servToggle), row.names=FALSE)

}



