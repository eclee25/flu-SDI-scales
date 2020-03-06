
## Name: Elizabeth Lee
## Date: 5/12/19
## Function: look at distribution of error in top 10% most populous counties vs other counties
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_msFigs.R")
source("source_import_modeldata.R")

################################
dbCodeStr <- "_irDt_Octfit_span0.4_degree2"

## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
path_latlon_reg <- paste0(getwd(), "/region_latlon.csv")
path_region_cw <- paste0(getwd(), "/state_abbreviations_FIPS_region.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_response_reg <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st, 
                  path_region_cw = path_region_cw,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_latlon_reg = path_latlon_reg,
                  path_response_st = path_response_st,
                  path_response_reg = path_response_reg,
                  path_response_cty = path_response_cty)

################################
#### MAIN ####
setwd(dirname(sys.frame(1)$ofile))

#### IMPORT DATA ####
datFormats_st <- list(offset_l = FALSE, bigscale = "st")
datFormats_reg <- list(offset_l = FALSE, bigscale = "reg")
datFormats_correlog <- list(dataScale = "cty", resamp = 500)

################################
pop_cty <- clean_pop_cty(path_list) 
top_pops <- pop_cty %>%
  group_by(st, season) %>%
  dplyr::mutate(top.1 = ifelse(pop >= quantile(pop, probs = c(.9)), TRUE, FALSE)) %>%
  ungroup %>%
  dplyr::filter(top.1) %>%
  distinct(fips) %>% unlist

aggBiasDat_st <- import_obs_aggBias_allMeasures(path_list, datFormats_st) %>%
  full_join(pop_cty %>% dplyr::select(fips, season, pop), by = c("fips", "season")) %>%
  dplyr::mutate(toppop = ifelse(fips %in% top_pops, TRUE, FALSE))

################################
#### EXPLORE POP DIFFERENCES ####
plt_bias_wksToEpi <- ggplot(aggBiasDat_st, aes(x = bias_wksToEpi)) +
  geom_histogram(aes(fill = toppop), position = position_dodge(), alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~season)
print(plt_bias_wksToEpi)

plt_bias_wksToPeak <- ggplot(aggBiasDat_st, aes(x = bias_wksToPeak)) +
  geom_histogram(aes(fill = toppop), position = position_dodge(), alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~season)
print(plt_bias_wksToPeak)

plt_bias_iliEarly <- ggplot(aggBiasDat_st, aes(x = bias_iliEarly)) +
  geom_histogram(aes(fill = toppop), position = position_dodge(), alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~season)
print(plt_bias_iliEarly)

plt_bias_iliPeak <- ggplot(aggBiasDat_st, aes(x = bias_iliPeak)) +
  geom_histogram(aes(fill = toppop), position = position_dodge(), alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~season)
print(plt_bias_iliPeak)

