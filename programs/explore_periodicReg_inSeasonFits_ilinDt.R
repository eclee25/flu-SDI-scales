
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: 1. explore periodic regression fits of detrended ilin (ilin/fitted.loess), colored by in season classifications, through plotting
## Filenames: 
## Data Source: 
## Notes: 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


explore_periodicReg_inSeasonFits_ilinDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
  
  ## uncomment when running script separately
  # spatial <- list(scale = "state", stringcode = "State", stringabbr = "_st", serv = "_emergency", servToggle = "_emergency", age = "_totAge", ageToggle = "")
  # span.var <- 0.4 # 0.4, 0.6
  # degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### plot formatting ################################
  w <- 9; h <- 6
  num <- 6
  
  #### import data ################################
  setwd('../R_export')
  
  if (spatial$scale == 'zip3'){
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, code.str, spatial$stringabbr), col_names = T, col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = zip3)
    
  } else if (spatial$scale == 'state'){
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s_analyzeDB%s.csv', code, code2, code.str, spatial$stringabbr), col_names = T, col_types = list(state = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = state)
  } else if (spatial$scale == 'county'){
    data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), col_names = T, col_types = list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = fips)
  }
  
  #### 9/15/15 in.season fits ################################
  print(sprintf('plotting in season time series %s', code.str))
  dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilinDt%s%s%s%s%s/inSeason', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), showWarnings = FALSE)
  setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilinDt%s%s%s%s%s/inSeason', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr))
  
  zip3list2 <- data5 %>% select(scale) %>% distinct(scale) %>% arrange(scale) %>% mutate(for.plot = seq_along(1:nrow(.)))
  data_plot2 <- right_join(data5, zip3list2, by="scale")
  indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=num)

  for(i in indexes2){
    dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=week, y=ilin.dt, group=scale)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(color = in.season)) +  
      scale_color_brewer(name = 'in.season', palette = 'Set1') +
      geom_line(aes(y=ifelse(flu.week, 1, NA)), color = 'black') + 
      geom_line(aes(y = .fitted), color = 'black') + 
      facet_wrap(~scale, scales="free_y")
    # grab zip3s in plot for file name
    ziplabels <- data_plot2 %>% select(scale) %>% distinct(scale) %>% slice(c(i, i+num-1)) 
    ggsave(sprintf("periodicReg_inSeas_%sfits_ilinDt%s%s_%s-%s.png", code, code2, code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
  } 
  
}