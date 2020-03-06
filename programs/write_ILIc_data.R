
## Name: Elizabeth Lee
## Date: 10/15
## Function: write ilic data: ilic_{z, w} = ili_{z, w} / alpha_{z, y} / effPhysCov_{z, y}
## ilic --> number of ili cases in zip z in week w, correcting for constant care-seeking rates across zip3s and scaling up for physician coverage; scaling up assumes that the ili/physician ratio is the same for the reported and unreported cases
## alpha_{z, y} = (viz_{z, y}/numPhys_{z, y}) / (\bar{viz_y}/\bar{phys_y}) --> correction for general care-seeking behavior
## Filenames: physician_coverage/DX_Coverage_by_Flu_Season_20150620.csv; Py_export/iliByallZip_allWeekly_totServ_totAge.csv
## Data Source: IMS Health ili dataset and physician coverage dataset
## Notes: 1/4/17 rm toggle for agegroup
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
# 11/10/16 new age group toggle; 1/4/17 migrate age group data export to wrote_ILIc_age_data.R
agegroup <- "totAge" # don't toggle

#### import data ################################
setwd('../Py_export')
ili_df <- read.csv(sprintf('iliByallZip_allWeekly_totServ_%s.csv', agegroup), header=T, colClasses=c("week"="Date"))
# viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
pop_df <- read.csv(sprintf('popByallZip_allYearly_%s.csv', agegroup), header=T, colClasses=c("year"="character"))

setwd('../R_export')
cov_df <- read_csv('physicianCoverage_IMSHealth_zip3.csv', col_types = list("zip3" = col_character()))

#### data cleaning: ILI data ####################################
# date formatting in ili dataset
ili_df2 <- ili_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = as.numeric(substr(as.character(Thu.week), 1, 4))) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 11 | month <= 4)) %>% 
  mutate(t = seq_along(1:nrow(.))) %>%
  mutate(fit.week = (month >= 4 & month <= 10))

# gather ili data
ili_gather_df <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE) %>% 
  mutate(week = as.Date(week, origin="1970-01-01")) %>%
  mutate(Thu.week = as.Date(Thu.week, origin="1970-01-01")) %>% 
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) %>%
  mutate(ili = ifelse(is.na(ili), 0, ili)) # 7/15/16: replace NAs with 0

# gather pop data
pop_gather_df <- gather(pop_df, zip3, pop, X2:X999, convert=FALSE) %>%
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) %>%
  mutate(year = as.numeric(substr(as.character(year), 1, 4)))

#### data cleaning: pop data ####################################
# clean pop data
numNAs <- pop_gather_df %>% filter(is.na(pop)) %>% group_by(zip3) %>% count(pop)
fillZips <- numNAs %>% filter(n <= 3) 

# process zips with 1, 2, and 3 missing pop data differently
# fill in pop data for locations with only 1-3 population years missing. these populations will be filled in with the same values as subsequent population data (e.g. if zip3 is missing pop data for 2001, fill in with zip3 pop data for 2002)

## 1 NA ##
fillZips1 <- fillZips %>% filter(n == 1) %>% select(zip3) %>% unlist
tofill1_df <- pop_gather_df %>%
  filter(zip3 %in% fillZips1) %>%
  filter(is.na(pop))
filler1_df <- tofill1_df %>%
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(pop_gather_df, by = c("year", "zip3")) %>%
  mutate(year = year - 1)

## 2 NAs ##
fillZips2 <- fillZips %>% filter(n == 2) %>% select(zip3) %>% unlist
tofill2_df <- pop_gather_df %>%
  filter(zip3 %in% fillZips2) %>%
  filter(is.na(pop))
# later years will be filled by original pop dataset
tofill2_df_a <- tofill2_df %>%
  group_by(zip3) %>%
  filter(year == max(year))
filler2_df_a <- tofill2_df_a %>% 
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(pop_gather_df, by = c("year", "zip3")) %>%
  mutate(year = year - 1)
# earlier years will be filled by newly filled or original pop dataset
tofill2_df_b <- tofill2_df %>%
  group_by(zip3) %>%
  filter(year == min(year))
tempPop <- pop_gather_df %>% 
  filter(!is.na(pop)) %>%
  bind_rows(filler2_df_a)
filler2_df_b <- tofill2_df_b %>%
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(tempPop, by = c("year", "zip3")) %>%
  mutate(year = year - 1)
# bind the two sets of NAs
filler2_df <- bind_rows(filler2_df_a, filler2_df_b) %>%
  arrange(zip3, year)

## 3 NAs ##
fillZips3 <- fillZips %>% filter(n == 3) %>% select(zip3) %>% unlist
tofill3_df <- pop_gather_df %>%
  filter(zip3 %in% fillZips3) %>%
  filter(is.na(pop))
# later years will be filled by original pop dataset
tofill3_df_a <- tofill3_df %>%
  group_by(zip3) %>%
  filter(year == max(year))
filler3_df_a <- tofill3_df_a %>% 
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(pop_gather_df, by = c("year", "zip3")) %>%
  mutate(year = year - 1)
# middling years will be filled by newly filled or original pop dataset
tofill3_df_b <- tofill3_df %>%
  group_by(zip3) %>%
  filter(year != min(year) & year != max(year))
tempPop3 <- pop_gather_df %>% 
  filter(!is.na(pop)) %>%
  bind_rows(filler3_df_a)
filler3_df_b <- tofill3_df_b %>%
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(tempPop3, by = c("year", "zip3")) %>%
  mutate(year = year - 1)
# earlier years will be filled by newly filled or original pop dataset
tofill3_df_c <- tofill3_df %>%
  group_by(zip3) %>%
  filter(year == min(year))
tempPop4 <- tempPop3 %>%
  bind_rows(filler3_df_b)
filler3_df_c <- tofill3_df_c %>%
  mutate(year = year + 1) %>%
  select(-pop) %>%
  left_join(tempPop4, by = c("year", "zip3")) %>%
  mutate(year = year - 1)
# bind the two sets of NAs
filler3_df <- bind_rows(filler3_df_a, filler3_df_b, filler3_df_c) %>%
  arrange(zip3, year)

# merge all pop data #
pop_data <- pop_gather_df %>%
  filter(!is.na(pop)) %>%
  bind_rows(filler3_df, filler2_df, filler1_df) %>%
  arrange(zip3, year)

# merge ili & pop data
iliDat <- left_join(ili_gather_df, pop_data, by = c('zip3', 'year'))

#### data cleaning: physician coverage data ####################################
# drop columns in cov data & generate alpha.numer
cov_df2 <- cov_df %>% select(zip3, year, avgUnvrs, sampViz, covAdjProv) %>%
  filter(!is.na(sampViz + avgUnvrs)) %>% 
  mutate(alpha.numer = sampViz/avgUnvrs) %>%
  rename(cov_z.y = covAdjProv) %>%
  mutate(cov_below5 = cov_z.y < 0.05)

# calculate \bar{viz_y}/\bar{phys_y})
mn_viz_phys <- cov_df2 %>% 
  group_by(year) %>%
  summarise(bar.viz_y = mean(sampViz), bar.phys_y = mean(avgUnvrs)) %>%
  mutate(alpha.denom = bar.viz_y/bar.phys_y) %>%
  ungroup

# calculate alpha_{z, y}
alphaDat_Full <- left_join(cov_df2, mn_viz_phys, by = 'year') %>%
  mutate(alpha_z.y = alpha.numer/alpha.denom) %>%
  rename(viz_z.y = sampViz) %>%
  rename(phys_z.y = avgUnvrs)

### copy cov & alpha data from 2002 to 2001 weeks because coverage data begins in 2002 ###
dummy2001Dat <- alphaDat_Full %>% filter(year == 2002) %>%
  mutate(year = 2001)
alphaDat_Full2 <- bind_rows(alphaDat_Full, dummy2001Dat)
alphaDat_Full2 <- arrange(alphaDat_Full2, zip3, year)

# shorten dataset
alphaDat <- alphaDat_Full2 %>% select(zip3, year, cov_z.y, alpha_z.y, cov_below5) 

#### data cleaning: write ilic data ####################################
# archive: identify zip3s with too little ILI data during fitted week periods (Apr to Oct >= 50 NAs)
# 7/15/16: identify zip3s with NA as pop; these will be removed from the dataset
iliDat2 <- iliDat %>%
  mutate(incl.lm = ifelse(is.na(pop), FALSE, TRUE))

# create ilic metric
ilicDat <- left_join(iliDat2, alphaDat, by = c("zip3", "year")) %>%
  mutate(ILIc = ili/alpha_z.y/cov_z.y)

#### write Data to file ####################################
write.csv(alphaDat_Full2, file = 'vizPhysRatio_zipYear_corrections.csv', row.names=F)
write.csv(ilicDat, file = sprintf('ilicByallZip3_allWeekly_totServ_%s.csv', agegroup), row.names=F)
# exported 7/16/16








