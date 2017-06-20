
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: 1. explore periodic regression fits of detrended ilin (ilin/fitted.loess) through plotting
## Filenames: 
## Data Source: 
## Notes: 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


explore_periodicReg_fits_ilinDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
  
#   ## uncomment when running script separately
#   spatial <- list(scale = "county", stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "")
#   span.var <- 0.4 # 0.4, 0.6
#   degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### plot formatting ################################
  w <- 9; h <- 6
  num <- 6
  
  #### import data ################################
  setwd('../R_export')
  
  if (spatial$scale == 'zip3'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, code.str), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = zip3)
    fitdata <- read_csv(file=sprintf('summaryStats_periodicReg_%sall%sMods_ilinDt%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, code.str), col_types=list(zip3 = col_character())) %>%
      rename(scale = zip3)
    
  } else if (spatial$scale == 'state'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s.csv', code, spatial$stringcode, code2, code.str), col_types=list(state = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = state)
    fitdata <- read_csv(file=sprintf('summaryStats_periodicReg_%sall%sMods_ilinDt%s%s.csv', code, spatial$stringcode, code2, code.str), col_types=list(state = col_character())) %>%
      rename(scale = state)
  } else if (spatial$scale == 'county'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, spatial$ageToggle, code.str), col_types=list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = fips)
    fitdata <- read_csv(file=sprintf('summaryStats_periodicReg_%sall%sMods_ilinDt%s%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, spatial$ageToggle, code.str), col_types=list(fips = col_character())) %>%
      rename(scale = fips)
  }
  
  #### initial time series plots ################################
  print(sprintf('plotting time series %s', code.str))
  dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilinDt%s%s%s%s%s', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), showWarnings = FALSE)
  setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilinDt%s%s%s%s%s', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr))
  
  zip3list <- data %>% filter(incl.lm) %>% select(scale) %>% distinct(scale) %>% mutate(for.plot = seq_along(1:nrow(.)))
  data_plot <- right_join(data, zip3list, by="scale")
  indexes <- seq(1, max(data_plot %>% select(for.plot)), by=num)

  for(i in indexes){
    dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+num), aes(x=week, y=ilin.dt, group=scale)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit: Apr to Oct)') + 
      geom_line(aes(y = .fitted), color = 'black') + 
      geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
      scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
      facet_wrap(~scale, scales="free_y")
    # grab zip3s in plot for file name
    ziplabels <- data_plot %>% select(scale) %>% distinct(scale) %>% slice(c(i, i+num-1)) 
    ggsave(sprintf("periodicReg_%sfits_ilinDt%s%s_%s-%s.png", code, code2, code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
  } 
  
  #### 10/20/15 residual vs fitted ################################
  print(sprintf('plotting diagnostics %s', code.str))
  dir.create('./diagnostics', showWarnings = FALSE)
  setwd('./diagnostics')
  
  data_plot2 <- data_plot %>% filter(fit.week) %>% mutate(resid = ilin.dt - .fitted) %>% mutate(data95indic = ifelse(ilin.dt > .fitted-(1.96*.se.fit) & ilin.dt < .fitted+(1.96*.se.fit), TRUE, FALSE))
  indexes2 <- indexes
  
  for (i in indexes2){
    dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=.fitted, y=resid, group=scale)) +
      geom_point(aes(colour = data95indic)) +
      scale_colour_discrete(name = 'In 95%CI of model fit') + # Do 95% of observed data fit within the 95%CI?
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      facet_wrap(~scale, scales="free") +
      xlab('Fitted Values (fit.week = T)') +
      ylab('Residuals (obs - fitted)')
    # grab zip3s in plot for file name
    ziplabels <- data_plot %>% select(scale) %>% distinct(scale) %>% slice(c(i, i+num-1)) 
    ggsave(sprintf("periodicReg_%sresidualsVsFitted_ilinDt%s%s_%s-%s.png", code, code2, code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
  } 
  
  #### 10/26/15 histogram of adjusted R^2 ################################
  quantD <- fitdata %>% summarise(q05 = quantile(adj.r.squared, 0.05), q25 = quantile(adj.r.squared, 0.25), q50 = quantile(adj.r.squared, 0.5), q75 = quantile(adj.r.squared, 0.75), q95 = quantile(adj.r.squared, 0.95)) %>% gather(quant, val, q05:q95)
  
  adjR2plot <- ggplot(fitdata, aes(x = adj.r.squared)) +
    geom_histogram(na.rm=TRUE) + 
    geom_vline(data = quantD, aes(xintercept = val), colour = 'red')
  ggsave(sprintf("periodicReg_%sAdjR2_ilinDt%s%s.png", code, code2, code.str), adjR2plot, width=w, height=h)
 
  #### 10/27/15 histogram of sigma hat (unexplained variance) ################################
  sigmaplot <- ggplot(fitdata, aes(x = sigma)) +
    geom_histogram(binwidth = 0.2) +
    coord_cartesian(xlim = c(0, 2))
  ggsave(sprintf("periodicReg_%ssigma_ilinDt%s%s.png", code, code2, code.str), sigmaplot, width=w, height=h)
  
  print(fitdata %>% filter(sigma>0.9))
}




