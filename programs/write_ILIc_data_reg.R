
## Name: Elizabeth Lee
## Date: 9/8/17
## Function: write ili data at region level
## Use this as another counterpoint for cty-level comparison of aggregation bias

## Filenames: physician_coverage/DX_Coverage_by_Flu_Season_20150620.csv; Py_export/iliByallZip_allWeekly_totServ_totAge.csv
## Data Source: IMS Health ili dataset and physician coverage dataset
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))

#### import data ################################
setwd('../Py_export')
ilizip_df <- read.csv('iliByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
popzip_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

setwd('../reference_data')
# zip3-state crosswalk
state_cw <- read_csv(file="Coord3digits.csv", col_types=list(zip3=col_character(), st_FIPS=col_character()))
state_cw$zip3 <- substr.Right(paste0("00", state_cw$zip3), 3)
state_cw$st_FIPS <- substr.Right(paste0("00", state_cw$st_FIPS), 2)
# state-region crosswalk
reg_cw <- read_csv(file="state_abbreviations_FIPS_region.csv", "c__i", skip=1, col_names = c("st_FIPS", "region"))
# process and merge crosswalk data: 1) drop irrelevant variables 
reg_cw2 <- right_join(state_cw, reg_cw, by = c("st_FIPS")) %>% 
  select(zip3, STATE, st_FIPS, region)

# create region pop data
reg_pop <- read_csv('pop_st_Census_00-10.csv', col_types = 'c_ii') %>%
  rename(st_FIPS = st_fip) %>%
  left_join(reg_cw, by = c("st_FIPS")) %>%
  group_by(region, year) %>%
  summarise(pop = sum(pop))

setwd('../R_export')
cov_df <- read_csv('physicianCoverage_IMSHealth_region.csv')

#### plot formatting ################################
w <- 8; h <- 6; dp <- 300

#### data cleaning: ILI data ####################################
# date formatting in ili dataset
ili_df2 <- ilizip_df %>% 
  mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = as.numeric(substr(as.character(Thu.week), 1, 4))) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 11 | month <= 4)) %>% 
  mutate(t = seq_along(1:nrow(.))) %>%
  mutate(fit.week = (month >= 4 & month <= 10))

# gather ili data
ili_gather_df <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE) %>% 
  mutate(week = as.Date(week, origin="1970-01-01")) %>%
  mutate(Thu.week = as.Date(Thu.week, origin="1970-01-01")) %>% 
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) 

# group ili data to the region level
# 1) join reg crosswalk to cleaned data3; 2) drop data where state is NA or 0; 3) drop season
iliGather_reg <- left_join(ili_gather_df, reg_cw2, by="zip3") %>% 
  filter(!is.na(region)) %>% 
  group_by(Thu.week, region) %>%
  summarise(week = first(week), year = first(year), month = first(month), flu.week = first(flu.week), t = first(t), fit.week = first(fit.week), ili = sum(ili, na.rm=T)) %>%
  ungroup %>%
  select(week, Thu.week, year, month, flu.week, t, fit.week, region, ili)

# merge ili & pop data
iliDat <- left_join(iliGather_reg, reg_pop, by = c('region', 'year')) 

#### EDA: state-level data ####################################
dir.create("../graph_outputs/explore_allILI_reg_ts", showWarnings = FALSE)
setwd("../graph_outputs/explore_allILI_reg_ts")

# 12/10/15
# plot raw ili at state level
eda.ili <- ggplot(iliDat, aes(x = Thu.week, y = ili)) +
  geom_line() +
  facet_wrap(~region) +
  ylab("ILI cases")
ggsave("ili_region_ts.png", eda.ili, width = w, height = h, dpi = dp)

# plot rate of ILI per 10,000 at state level
eda.ilirate <- ggplot(iliDat %>% mutate(iliRate = ili/pop*10000), aes(x = Thu.week, y = iliRate)) +
  geom_line() +
  facet_wrap(~region) + 
  ylab("ILI rate per 10,000 population") + coord_cartesian(ylim = c(0, 25))
ggsave("ilin_region_ts.png", eda.ilirate, width = w, height = h, dpi = dp)

# 9/8/17 rm physician coverage data cleaning (as compared to write_ILIc_data.R)


#### data cleaning: create export data ####################################
# drop columns in cov data & generate alpha.numer
cov_df2 <- cov_df %>% select(eval(parse(text = "region")), year, maxUnvrs, sampViz, covAdjProv) %>%
  filter(!is.na(sampViz + maxUnvrs)) %>% 
  mutate(alpha.numer = sampViz/maxUnvrs) %>%
  rename(cov_z.y = covAdjProv) %>%
  mutate(cov_below5 = cov_z.y < 0.05)

# calculate \bar{viz_y}/\bar{phys_y})
mn_viz_phys <- cov_df2 %>% 
  group_by(year) %>%
  summarise(bar.viz_y = mean(sampViz), bar.phys_y = mean(maxUnvrs)) %>%
  mutate(alpha.denom = bar.viz_y/bar.phys_y) %>%
  ungroup

# calculate alpha_{z, y}
alphaDat_Full <- left_join(cov_df2, mn_viz_phys, by = 'year') %>%
  mutate(alpha_z.y = alpha.numer/alpha.denom) %>%
  rename(viz_z.y = sampViz) %>%
  rename(phys_z.y = maxUnvrs)

### copy cov & alpha data from 2002 to 2001 weeks because coverage data begins in 2002 ###
dummy2001Dat <- alphaDat_Full %>% filter(year == 2002) %>%
  mutate(year = 2001)
alphaDat_Full2 <- bind_rows(alphaDat_Full, dummy2001Dat)
alphaDat_Full2 <- arrange(alphaDat_Full2, region, year)

# shorten dataset
alphaDat <- alphaDat_Full2 %>% select(eval(parse(text = "region")), year, cov_z.y, alpha_z.y, cov_below5)

#### data cleaning: generate ilic data ####################################
# create ilic metric
iliDat2 <- iliDat %>% mutate(incl.lm = TRUE) %>%
  left_join(alphaDat, by = c("region", "year")) %>%
  mutate(ILIc = ili/alpha_z.y/cov_z.y) %>%
  arrange(region, Thu.week) %>%
  mutate(region = paste0("R", region))

#### write Data to file ####################################
setwd("../../R_export")
write.csv(iliDat2, file = 'ilicByallRegion_allWeekly_totServ_totAge.csv', row.names=F)
# exported 10/1/17








