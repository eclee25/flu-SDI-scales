
## Name: Elizabeth Lee
## Date: 9/8/17
## Function: write ilic data at region level: ilic_{z, w} = ili_{z, w} / alpha_{z, y} / effPhysCov_{z, y}
# IR data: IR = iliProp * pop/100000
## iliProp = proportion of visits due to ILI
## ilic --> number of ili cases in state s in week w, correcting for constant care-seeking rates across states and scaling up for physician coverage; scaling up assumes that the ili/physician ratio is the same for the reported and unreported cases
## alpha_{z, y} = (viz_{z, y}/numPhys_{z, y}) / (\bar{viz_y}/\bar{phys_y}) --> correction for general care-seeking behavior
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
viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
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

regionDf <- data.frame(region = 1:10, regionName = c("Boston", "New York", "Philadelphia", "Atlanta", "Chicago", "Dallas", "Kansas City", "Denver", "San Francisco", "Seattle"))

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

#### data cleaning: visit data ####################################
viz_df2 <- viz_df %>% mutate(Thu.week = as.Date(week+4))
viz_gather_df <- gather(viz_df2, zip3, viz, X2:X999, convert=FALSE) %>%
  select(-week) %>%
  mutate(Thu.week = as.Date(Thu.week, origin="1970-01-01")) %>% 
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3))  

merge_gather_df <- full_join(ili_gather_df, viz_gather_df, by = c("Thu.week", "zip3")) %>%
  mutate(ili = ifelse(is.na(ili), 0, ili)) %>% # 7/15/16: replace NAs with 0
  mutate(viz = ifelse(is.na(viz), 0, viz)) %>%
  mutate(iliProp = ifelse(ili==0 & viz==0, 0, ili/viz)) %>% # checked that no records have more ili than visits
  arrange(zip3, Thu.week)


# group ili data to the region level
# 1) join reg crosswalk to cleaned data3; 2) drop data where state is NA or 0; 3) drop season
iliGather_reg <- left_join(merge_gather_df, reg_cw2, by="zip3") %>% 
  filter(!is.na(region)) %>% 
  group_by(Thu.week, region) %>%
  summarise(week = first(week), year = first(year), month = first(month), flu.week = first(flu.week), t = first(t), fit.week = first(fit.week), ili = sum(ili, na.rm=T), viz = sum(viz, na.rm=T), iliProp = sum(iliProp, na.rm=T)) %>%
  ungroup %>%
  select(week, Thu.week, year, month, flu.week, t, fit.week, region, ili, viz, iliProp)

# merge ili & pop data
iliDat <- left_join(iliGather_reg, reg_pop, by = c('region', 'year')) 

#### data cleaning: create export data ####################################
# drop columns in cov data & generate alpha.numer
cov_df2 <- cov_df %>% select(region, year, maxUnvrs, sampViz, covAdjProv) %>%
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
alphaDat <- alphaDat_Full2 %>% select(region, year, cov_z.y, alpha_z.y, cov_below5)

#### data cleaning: generate ilic data ####################################
# create ilic metric
iliDat2 <- iliDat %>% 
  left_join(alphaDat, by = c("region", "year")) %>%
  mutate(incl.lm = ifelse(is.na(pop)|is.na(iliProp), FALSE, TRUE)) %>%
  mutate(ILIc = ili/alpha_z.y/cov_z.y) %>%
  mutate(IR = iliProp * pop/100000) %>%
  arrange(region, Thu.week) %>%
  full_join(regionDf, by = c("region")) %>%
  mutate(region = paste0("R", region))

#### EDA: state-level data ####################################
dir.create("../graph_outputs/explore_allILI_reg_ts", showWarnings = FALSE)
setwd("../graph_outputs/explore_allILI_reg_ts")

# 12/10/15
# plot raw ili at state level
eda.ili <- ggplot(iliDat2, aes(x = Thu.week, y = IR)) +
  geom_line() +
  facet_wrap(~regionName) +
  ylab("IR (per 100K)")
ggsave("IR_region_ts.png", eda.ili, width = w, height = h, dpi = dp)

# plot rate of ILI per 10,000 at state level
eda.ilirate <- ggplot(iliDat2, aes(x = Thu.week, y = iliProp)) +
  geom_line() +
  facet_wrap(~regionName) + 
  ylab("ILI as proportion of visits")
ggsave("iliProp_region_ts.png", eda.ilirate, width = w, height = h, dpi = dp)

# 9/8/17 rm physician coverage data cleaning (as compared to write_ILIc_data.R)


#### write Data to file ####################################
setwd("../../R_export")
write_csv(iliDat2, path = 'ilicPropByallRegion_allWeekly_totServ_totAge.csv')
# exported 10/1/17

write_csv(alphaDat_Full2, path = 'vizPhysRatio_regionYear_corrections.csv') # should be the same as write_ILIc_data.R so don't need to write it again







