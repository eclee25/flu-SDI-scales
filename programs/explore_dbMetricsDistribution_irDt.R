## Name: Elizabeth Lee
## Date: 10/14/17
## Function: explore distributions of disease burden metrics for irDt
## Results: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
### disease burden metrics: sum ILI across epidemic weeks, cumulative difference in ILI and baseline, cumulative difference in ILI and epidemic threshold, rate of ILI at epidemic peak, epidemic duration, time to epidemic from start of flu period, time to epidemic peak from start of epidemic
## Filenames: sprintf('dbMetrics_periodicReg_%sirDt%s_analyzeDB.csv', code, code2)
## Data Source: IMS Health 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

explore_dbMetricsDistribution_irDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
  #### header ####################################
  require(tidyverse)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
  
  ## uncomment when running script separately
  # spatial <- list(scale = "state", stringcode = "State", stringabbr = "_st", serv = "_emergency", servToggle = "_emergency")
  # span.var <- 0.4 # 0.4, 0.6
  # degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### import data ####################################
  setwd('../R_export')
  dbMetrics.g <- read_csv(sprintf('dbMetrics_periodicReg_%sirDt%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), col_types="icllcd") %>% filter(season!=1)
  # standardized data
  dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden, na.rm=TRUE))/sd(burden, na.rm=TRUE))
  
  #### plot formatting ####################################
  w = 9
  h = 6
  
  #### plot distribution of dbMetrics ####################################
  print(sprintf('plotting db metrics %s', code.str))
  # 7/18/16 - saved figures
  dir.create(sprintf('../graph_outputs/explore_dbMetricsDistribution_%sirDt%s%s%s%s%s', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr), showWarnings=FALSE)
  setwd(sprintf('../graph_outputs/explore_dbMetricsDistribution_%sirDt%s%s%s%s%s', code, code2, spatial$servToggle, spatial$ageToggle, code.str, spatial$stringabbr))
  
  # total ILI plot
  plt.distr.iliSum <- ggplot(dbMetrics.g %>% filter(metric=='irDt.sum'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(0, 1)) +
    facet_wrap(~season) + ggtitle("Sum irDt during flu season")
  ggsave(sprintf("distr_ILITot_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.iliSum, width=w, height=h)

  # early ILI plot
  plt.distr.iliEarly <- ggplot(dbMetrics.g %>% filter(metric=='irDt.early'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    coord_cartesian(xlim=c(0, 1)) +
    facet_wrap(~season) + ggtitle("Sum irDt during early flu season")
  ggsave(sprintf("distr_ILIearly_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.iliEarly, width=w, height=h)
  
  # ILI in excess of modeled seasonal baseline
  plt.distr.ILIexcessBL <- ggplot(dbMetrics.g %>% filter(metric=='irDt.excess.BL'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(0, 1)) +
    facet_wrap(~season) + ggtitle("irDt in excess of modeled seasonal baseline during flu season")
  ggsave(sprintf("distr_ILIexcessBL_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.ILIexcessBL, width=w, height=h)
  
  # ili in excess of modeled epidemic threshold (BL + 1.96*se)
  plt.distr.ILIexcessThresh <- ggplot(dbMetrics.g %>% filter(metric=='irDt.excess.thresh'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(0, 1)) +
    facet_wrap(~season) + ggtitle("irDt in excess of modeled epidemic threshold during flu season")
  ggsave(sprintf("distr_ILIexcessThresh_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.ILIexcessThresh, width=w, height=h)
  
  # ili peak case count plot
  plt.distr.pkCount <- ggplot(dbMetrics.g %>% filter(metric=='irDt.peak'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=5) + geom_density() + 
    coord_cartesian(xlim=c(0, 1)) +
    facet_wrap(~season) + ggtitle("peak irDt count during flu season")
  ggsave(sprintf("distr_pkCount_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.pkCount, width=w, height=h)
  
  # epidemic duration plot
  plt.distr.epiDur <- ggplot(dbMetrics.g %>% filter(metric=='epi.dur'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("epidemic duration (weeks) during flu season")
  ggsave(sprintf("distr_epiDur_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.epiDur, width=w, height=h)
  
  # epidemic timing plot
  plt.distr.epiTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.epi'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("weeks to epidemic start during flu season")
  ggsave(sprintf("distr_epiTime_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.epiTime, width=w, height=h)
  
  # peak timing plot
  plt.distr.pkTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.peak'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("weeks to peak during epidemic")
  ggsave(sprintf("distr_pkTime_%sirDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.pkTime, width=w, height=h)
  
  print('finished plotting db metrics')
  # FINDING: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
  ####################################
  # compare the mean and variance for each metric by season
  metric.summ <- dbMetrics.g %>% group_by(season, metric) %>% summarise(MN = mean(burden, na.rm=TRUE), VAR = var(burden, na.rm=TRUE))
  print(sprintf('span %s degree %s', span.var, degree.var))
  print(metric.summ)
  
}



