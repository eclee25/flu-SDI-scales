# 7/6/17
# gut checks: write ILIn disease burden for model input in same format as other dbMetrics datasets
##########################################

require(tidyverse)

setwd(dirname(sys.frame(1)$ofile))

loessSettings <- "_span0.4_degree2"
dbCodeStr1 <- paste0("_ilinDt_Octfit", loessSettings)
dbCodeStr2 <- "_iliRate"

#### paths ####
setwd("../R_export")
# season fips has.epi incl.analysis metric burden
path_import_format <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr1))

path_import_data <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr1))

path_export <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr2))

#### import data ####
dbMetricFormat <- read_csv(path_import_format) %>%
  select(-burden, -metric) %>%
  distinct(season, fips, has.epi, incl.analysis) %>%
  mutate(metric = "iliRate")

dbMetricData <- read_csv(path_import_data, col_types = cols_only(fips = "c", season = "i", has.epi = "l", incl.analysis = "l", in.season = "l", ILIn = "d"))

#### process data ####
dbDat <- dbMetricData %>%
  filter(in.season) %>%
  group_by(fips, season) %>%
  summarise(burden = sum(ILIn, na.rm = TRUE))

# merge data
exportDat <- full_join(dbMetricFormat, dbDat, by = c("fips", "season")) %>%
  mutate(burden = ifelse(is.na(burden), 0, burden))
write_csv(exportDat, path_export)