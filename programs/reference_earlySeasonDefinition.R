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

setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")

fullDat <- read_csv("fullIndicAll_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_nat.csv")

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

setwd("../reference_data")
write_csv(inEarlySeasonDat, "earlySeasonDefinition.csv")
# 9/8/17