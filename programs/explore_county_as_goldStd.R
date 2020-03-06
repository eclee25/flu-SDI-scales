## compare the variance in time series between the state and county scales
## use this to explain why county should be the gold standard

library(tidyverse)
setwd(dirname(sys.frame(1)$ofile))

#### set these ################################
dbCodeStr <- "_irDt_Octfit_span0.4_degree2"


#### PATHS ##################################
setwd("../R_export")
path_ts_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_ts_st <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_st.csv", dbCodeStr))

ctyDat <- read_csv(path_ts_cty)
stDat <- read_csv(path_ts_st)

pltDat_cty <- ctyDat %>%
  dplyr::filter(!is.na(ir.dt)) %>%
  group_by(fips) %>%
  summarise(var = var(ir.dt)) %>%
  dplyr::rename(level = fips) %>%
  dplyr::mutate(datalevel = "county")
pltDat_st <- stDat %>%
  dplyr::filter(!is.na(ir.dt)) %>%
  group_by(fips_st) %>%
  summarise(var = var(ir.dt)) %>%
  dplyr::rename(level = fips_st) %>%
  dplyr::mutate(datalevel = "state")

pltDat <- bind_rows(pltDat_cty, pltDat_st)

plt <- ggplot(pltDat, aes(x = datalevel, y = var)) +
  geom_violin() +
  stat_summary(fun.y = "median", geom = "point", size = 2) +
  scale_y_continuous("Variance") +
  scale_x_discrete("Time series scale") +
  theme_bw() +
  theme(axis.title.x = element_blank())

print(plt)

