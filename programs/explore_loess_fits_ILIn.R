
## Name: Elizabeth Lee
## Date: 12/12/15
## Function: explore loess regression fits of ILIn through plotting; the loess fits will serve as a baseline to process the ILIn data, removing time trends
## Filenames: R_export/sprintf('loess%s_all%sMods_ILIn.csv', code.str, spatial$stringcode)
## Data Source: 
## Notes: 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

explore_loess_fits_ILIn <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  #   # uncomment when running script separately
#     spatial <- list(scale = "state", stringcode = "State", stringabbr = "_st", serv = "_emergency", servToggle = "_emergency", age = "_totAge", ageToggle = "")
#     span.var <- 0.4 # 0.4, 0.6
#     degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### plot formatting ################################
  w <- 9; h <- 6
  
  #### import data ################################
  setwd('../R_export')
  
  if (spatial$scale == 'zip3'){
    data <- read_csv(file=sprintf('loess%s_all%sMods_ILIn%s.csv', code.str, spatial$stringcode, spatial$servToggle), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(),  ILIn = col_double(), .fitted=col_double(), .se.fit=col_double(), ilin.dt=col_double(), ILIn = col_double())) %>%
      rename(scale = zip3)
  } else if (spatial$scale == 'state'){
    data <- read_csv(file=sprintf('loess%s_all%sMods_ILIn.csv', code.str, spatial$stringcode), col_types=list(state = col_character(), ili = col_integer(), pop = col_integer(),  ILIn = col_double(), .fitted=col_double(), .se.fit=col_double(), ilin.dt=col_double(), ILIn = col_double())) %>%
      rename(scale = state)
  } else if (spatial$scale == 'county'){
    data <- read_csv(file=sprintf('loess%s_all%sMods_ILIn%s%s.csv', code.str, spatial$stringcode, spatial$servToggle, spatial$ageToggle), col_types=list(fips = col_character(), ili = col_double(), pop = col_integer(),  ILIn = col_double(), .fitted=col_double(), .se.fit=col_double(), ilin.dt=col_double(), ILIn = col_double())) %>%
      rename(scale = fips)
    
  }
  
  
  #### loess fit plots ################################
  print('plotting loess fits')
  dir.create(sprintf('../graph_outputs/explore_loess_fits_ILIn%s%s%s', spatial$servToggle, spatial$ageToggle, spatial$stringabbr), showWarnings = FALSE)
  dir.create(sprintf('../graph_outputs/explore_loess_fits_ILIn%s%s%s/fits%s', spatial$servToggle, spatial$ageToggle, spatial$stringabbr, code.str), showWarnings = FALSE)
  setwd(sprintf('../graph_outputs/explore_loess_fits_ILIn%s%s%s/fits%s', spatial$servToggle, spatial$ageToggle, spatial$stringabbr, code.str))
  
  
  test <- tbl_df(data.frame(a = c(1, 1, 1, 2, 2), b= c(1, 1, 2, 2, 2)))
  zip3list <- data %>% filter(incl.lm) %>% select(scale) %>% distinct(scale) %>% mutate(for.plot = seq_along(1:nrow(.)))
  data_plot <- right_join(data, zip3list, by = "scale") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))
  indexes <- seq(1, max(data_plot %>% select(for.plot)), by=6)
  
  
  for(i in indexes){
    dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=Thu.week, y=ILIn, group=scale)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit: Apr to Oct)') + 
      geom_line(aes(y = .fitted), color = 'black') + 
      geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
      scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
      facet_wrap(~scale, scales="free_y")
    # grab zip3s in plot for file name
    ziplabels <- data_plot %>% select(scale) %>% distinct(scale) %>% slice(c(i, i+5)) 
    ggsave(sprintf("loess%s_fits_ILIn_%s-%s.png", code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
  }
  
  #### ilin.dt plots ################################
  print('plotting ilin.dt ts')
  dir.create(sprintf('../ilinDt%s', code.str), showWarnings = FALSE)
  setwd(sprintf('../ilinDt%s', code.str))
  
  for(i in indexes[1:5]){
    dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=Thu.week, y=ilin.dt, group=scale)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(color = fit.week)) + 
      facet_wrap(~scale, scales="free_y")
    # grab zip3s in plot for file name
    ziplabels <- data_plot %>% select(scale) %>% distinct(scale) %>% slice(c(i, i+5)) 
    ggsave(sprintf("ilinDt%s_data_%s-%s.png", code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
  } 
}



