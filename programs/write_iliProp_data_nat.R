
## Name: Elizabeth Lee
## Date: 9/8/17
## Function: write ili data at national level
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
viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
popzip_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

setwd('../reference_data')
# create national pop data
nat_pop <- read_csv('pop_st_Census_00-10.csv', col_types = 'c_ii') %>%
  group_by(year) %>%
  summarise(pop = sum(pop))

setwd('../R_export')

#### plot formatting ################################
w <- 8; h <- 6; dp <- 300

#### data cleaning: ILI data ####################################
# date formatting in ili dataset
ili_df2 <- ilizip_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
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

# group ili data to the national level
# 1) join reg crosswalk to cleaned data3; 2) drop data where state is NA or 0; 3) drop season
iliGather_nat <- merge_gather_df %>% 
  mutate(national = "N1") %>%
  group_by(Thu.week, national) %>%
  summarise(week = first(week), year = first(year), month = first(month), flu.week = first(flu.week), t = first(t), fit.week = first(fit.week), ili = sum(ili, na.rm=T), viz = sum(viz, na.rm=T)) %>%
  mutate(iliProp = ili/viz) %>%
  ungroup %>%
  select(week, Thu.week, year, month, flu.week, t, fit.week, national, ili, viz, iliProp)

# merge ili & pop data
iliDat <- left_join(iliGather_nat, nat_pop, by = c('year')) %>%
  mutate(IR = iliProp * 100)

#### EDA: national-level data ####################################
dir.create("../graph_outputs/explore_allILI_nat_ts", showWarnings = FALSE)
setwd("../graph_outputs/explore_allILI_nat_ts")

# 12/10/15
# plot raw ili at state level
eda.ili <- ggplot(iliDat, aes(x = Thu.week, y = IR)) +
  geom_line() +
  ylab("IR (per 100K)")
ggsave("IR_nat_ts.png", eda.ili, width = w, height = h, dpi = dp)

#### data cleaning: create export data ####################################
iliDat2 <- iliDat %>% mutate(incl.lm = TRUE) %>%
  arrange(Thu.week) 

#### write Data to file ####################################
setwd("../../R_export")
write.csv(iliDat2, file = 'iliPropByallNational_allWeekly_totServ_totAge.csv', row.names=F)
# exported 10/18/17








