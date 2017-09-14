## Name: Elizabeth Lee
## Date: 9/8/17
## Function: export a reference dataset identifying weeks within the "early season" for each year. This is defined as the four week period beginning the week before the in.season period for the processed national-level data
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
####################################
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")

fullDat <- read_csv("fullIndicAll_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_nat.csv")

setwd("../graph_outputs")
exportPath <- paste0(getwd(),"/reference_earlySeasonDefinition/")

#### local functions ################################
# define early season as period of maximum consecutive exponential growth, with an upper limit of 4 weeks
consider.expGrowth_maxconsec <- function(x){
    rle.results = rle(x)
    max.consec = max(0, rle.results$lengths[which(rle.results$values)]) # which(rle.results$values) works because values are boolean
    dummy <- rep(F, length(x))
    pre.index <- ((which(rle.results$values & rle.results$lengths==max.consec))[1])-1 # rle index of value prior to first value in longest consecutive run of TRUE
    post.index <- tail((which(rle.results$values & rle.results$lengths==max.consec)), n=1) # rle index of last value in longest consec run of TRUE 
    converted.pre.index <- ifelse(pre.index==1, 0, sum(rle.results$lengths[1:(pre.index-1)])) # vector index - 1 of first value in longest consec run of TRUE
    converted.post.index <- ifelse(post.index, ifelse(sum(rle.results$lengths[1:post.index]) <= converted.pre.index+3, sum(rle.results$lengths[1:post.index]), converted.pre.index+3), NA) # vector index of last value in longest consec run of TRUE, with max period of 3 weeks
    if(!is.na(converted.pre.index)){
      dummy[(converted.pre.index+1):converted.post.index] <- T
    }
    return(dummy) # full vector of T/F
  }

#### plot formatting ################################
w <- 5; h <- 5; dp <- 300

#### fixed at 4 week period beginning the week before the start of the epidemic period ################################
processDat <- fullDat %>%
    filter(season > 1 & in.season) %>%
    group_by(season) %>%
    mutate(t.firstearlyseasonweek = ifelse(Thu.week==min(Thu.week), t-1, 0)) %>%
    ungroup %>%
    select(season, t.firstearlyseasonweek) %>%
    filter(t.firstearlyseasonweek > 0) %>%
    mutate(t.lastearlyseasonweek = t.firstearlyseasonweek + 3)
  
inEarlySeasonDat <- left_join(fullDat, processDat, by = c("season")) %>%
    mutate(in.earlyseason = ifelse(t >= t.firstearlyseasonweek & t <= t.lastearlyseasonweek, TRUE, FALSE)) %>%
    filter(in.earlyseason) %>%
    select(season, Thu.week, in.earlyseason)

#### dynamically based on exponential growth ################################
# r must be positive and the period may endure only 2-3 weeks

n0Dat <- fullDat %>%
  filter(season > 2) %>%
  mutate(t = t+1) %>%
  rename(N0 = ilin.dt) %>%
  select(t, N0)

processDat2 <- full_join(fullDat, n0Dat, by = c("t")) %>%
  filter(season > 2 & in.season) %>%
  group_by(season) %>%
  mutate(tsteps = seq_along(ilin.dt)-1) %>%
  ungroup %>%
  mutate(expRate = ifelse(tsteps>0, log(ilin.dt/N0), NA)) %>%
  select(Thu.week, flu.week, ilin.dt, season, epi.thresh, epi.week, has.epi, in.season, N0, tsteps, expRate) %>%
  mutate(expGrowth = ifelse(expRate > 0, TRUE, FALSE)) %>%
  group_by(season) %>%
  mutate(in.earlyseason = consider.expGrowth_maxconsec(expGrowth)) %>%
  ungroup

inEarlySeasonDat2 <- processDat2 %>%
   filter(in.earlyseason) %>%
   select(season, Thu.week, in.earlyseason)

#### export reference datasets ################################

setwd("../reference_data")
write_csv(inEarlySeasonDat, "earlySeasonDefinition_4wks.csv")
write_csv(inEarlySeasonDat2, "earlySeasonDefinition_expRate.csv")

#### export plots ################################

iliplts <- ggplot(processDat2, aes(x = tsteps, y = ilin.dt, group = 1)) +
    geom_line(aes(colour = in.earlyseason)) +
    facet_wrap(~season) +
    theme_bw() + 
    theme(text = element_text(size = 12))
    ggsave(paste0(exportPath, "ilinDt_nat_bySeason.png"), iliplts, height = h, width = w, dpi = dp)

rplts <- ggplot(processDat2, aes(x = tsteps, y = expRate, group = 1)) +
    geom_line(aes(colour = in.earlyseason)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~season) +
    theme_bw() + 
    theme(text = element_text(size = 12))
    ggsave(paste0(exportPath, "r_nat_bySeason.png"), rplts, height = h, width = w, dpi = dp)
