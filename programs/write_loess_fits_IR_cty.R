
## Name: Elizabeth Lee
## Date: 10/1/17
## Function: downscale IR from zip3 to county, smooth ILIn data using loess smoother during summer weeks, divide observed ili by smoothed loess fits for entire time series. Pass the processed ILI metric to write_loess_PeriodicReg_....R
## timetrend <- loess(IR ~ t) during summer weeks 
## IRDt <- IR-timetrend
## lm(IRDt ~ t + cos(2*pi*t/52.18) + sin(2*pi*t/52.18))

## Filenames: 
## Data Source: 
## Notes: code2 = 'Octfit' --> October to April is flu period, but we fit the seasonal regression from April to October (i.e., expanded definition of summer) in order to improve phase of regression fits
## 52.18 weeks per year in the regression model
## 8/16/16 - forked from write_loess_fits_ILIn.R, separate downscaling procedure for ILIn (zip3 to county)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_loess_fits_IR <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  #### header ####################################
  require(tidyverse)
  require(broom)
  setwd(dirname(sys.frame(1)$ofile))
  # zip to county conversion functions
  source("source_clean_response_functions_cty.R")
  
  #### set these! ################################
#     # uncomment when running script separately
#     spatial <- list(scale = "county", stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "") 
#     span.var <- 0.4 # 0.4, 0.6
#     degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### import data ####################################
  setwd('../R_export')
  # import county-zip3 weights (source_clean_response_functions_cty.R)
  cw <- cw_zip3_cty()
  # import population data (source_clean_response_functions_cty.R)
  pop_data <- clean_pop_cty_plain()
  
  # import zip3 data
  zipILI_df <- read_csv(sprintf('ilicPropByallZip3_allWeekly%s%s.csv', spatial$serv, spatial$age), col_types = list(zip3 = col_character(), ili = col_double(), viz = col_double(), iliProp = col_double(), pop = col_double(), cov_z.y = col_double(), alpha_z.y = col_double(), cov_below5 = col_logical())) %>%
    select(week, Thu.week, year, month, flu.week, t, fit.week, zip3, ili, viz, iliProp, pop) 
  
  #### data cleaning ####################################
  # use population-weighted proportions to convert zip3 ILIn data to county
  ctyILI_df <- left_join(zipILI_df, cw, by = "zip3") %>%
    group_by(fips, Thu.week) %>% 
    summarise(week = first(week), year = first(year), month = first(month), flu.week = first(flu.week), t = first(t), fit.week = first(fit.week), ili = weighted.mean(ili, proportion, na.rm = TRUE), viz = weighted.mean(viz, proportion, na.rm = TRUE), iliProp = weighted.mean(iliProp, proportion, na.rm = TRUE)) %>%
    ungroup
  
  # merge with county pop data, re-create incl.lm (pop != NA)
  ilic_df2 <- left_join(ctyILI_df, pop_data, by = c("fips", "year")) %>%
    select(week, Thu.week, year, month, flu.week, t, fit.week, fips, iliProp, pop, ili, viz) %>%
    mutate(IR = iliProp*pop/100000) %>%
    mutate(incl.lm = ifelse(is.na(pop) | is.na(iliProp), FALSE, TRUE)) %>%
    rename(scale = fips) %>%
    filter(!is.na(scale)) #%>% # it appears that there are 496 scale NAs that are generated
#     mutate(ili = ifelse(ili < 1, 0, ili)) %>% # 8/17/16 `dataprocessing_backcalculateILI_ili1'
#     mutate(ILIn = ili/pop*100000)
  
#   # 8/17/16 `dataprocessing_backcalculateILI_ili1'
#   # change incl.lm to FALSE for counties with more than 300 weeks where ili=0
#   rmcty<- ilic_df %>%
#     mutate(zero = ifelse(ili == 0, 1, 0)) %>%
#     group_by(scale) %>%
#     summarise(zeros = sum(zero)) %>%
#     mutate(rm = ifelse(zeros >= 300, TRUE, FALSE)) 
#   ilic_df2 <- ilic_df %>%
#     full_join(rmcty, by = "scale") %>%
#     mutate(incl.lm = ifelse(rm==TRUE, FALSE, incl.lm)) %>%
#     select(-zeros, -rm)

  # create new data for augment
  newbasedata <- ilic_df2 %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01')) 
  
  #### perform loess regression ####################################
  allLoessMods <- ilic_df2 %>%
    filter(fit.week) %>% 
    filter(Thu.week < as.Date('2009-05-01')) %>% 
    filter(incl.lm) %>%
    group_by(scale) %>%
    do(fitCty = loess(IR ~ t, span = span.var, degree = degree.var, data = ., na.action=na.exclude))
    
  allLoessMods_aug <- augment(allLoessMods, fitCty, newdata= newbasedata)
 
  # after augment - join ILI data to fits
  # 7/18/16 incl.lm2 == .fitted>0
  allLoessMods_fit_ILI <- right_join((allLoessMods_aug %>% ungroup %>% select(-t)), (ilic_df2 %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'scale')) %>% 
    mutate(week=as.Date(week, origin="1970-01-01")) %>% 
    mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% 
    mutate(incl.lm2 = ifelse(IR >= .fitted, TRUE, FALSE)) %>%
    mutate(ir.dt = ifelse(incl.lm2, IR-.fitted, 0)) # 8/16/16 update: convert value to 0 if !incl.lm2; 7/28/16: don't make any change to detrending based on incl.lm2
  

  #### write data to file ####################################
  print('writing loess fits')
  setwd('../R_export')
  
  # rename scale variable
  allLoessMods_fit_ILI2 <- scaleRename(spatial$scale, allLoessMods_fit_ILI) 
  
  # write fitted and original loess smoothed ILI data 
  write.csv(allLoessMods_fit_ILI2, file=sprintf('loess%s_all%sMods_IR%s%s.csv', code.str, spatial$stringcode, spatial$servToggle, spatial$ageToggle), row.names=FALSE)

}



