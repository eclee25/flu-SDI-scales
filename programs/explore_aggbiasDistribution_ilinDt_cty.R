## Name: Elizabeth Lee
## Date: 7/15/17
## Function: explore distributions of disease burden metrics for ilinDt aggregation biases at the county level. Relative risk for measures with expected values.
## Results: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
### disease burden metrics: sum ILI across epidemic weeks, cumulative difference in ILI and baseline, cumulative difference in ILI and epidemic threshold, rate of ILI at epidemic peak, epidemic duration, time to epidemic from start of flu period, time to epidemic peak from start of epidemic
## Filenames: sprintf('dbMetrics_periodicReg_%silinDt%s_analyzeDB.csv', code, code2)
## Data Source: IMS Health 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


#### header ####################################
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

source("source_clean_response_functions_cty.R") # cleanR_iliSum_shift1_cty_aggBias
source("source_clean_response_functions_st.R") # cleanR_iliSum_shift1_st_aggBias
source("source_clean_response_functions_aggBias.R")
source("source_clean_data_functions.R")
source("source_prepare_inlaData_aggBias.R") # functions to import aggbias data
source("source_prepare_inlaData_cty.R") # functions to import response data from original models

#### set these! ####################################
code <-"" # linear time trend term
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

# uncomment when running script separately
spatial <- list(scale = "county", stringcode = "County", stringabbr = "_cty")
span.var <- 0.4 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                    path_latlon_cty = path_latlon_cty,
                    path_latlon_st = path_latlon_st,
                    path_shape_cty = path_shape_cty,
                    path_adjMxExport_cty = path_adjMxExport_cty,
                    path_response_cty = path_response_cty, 
                    path_response_st = path_response_st, 
                    path_graphIdx_cty = path_graphIdx_cty,
                    path_graphExport_st = path_graphExport_st,
                    path_graphIdx_st = path_graphIdx_st)

mod_list <- list(cty = "8a_iliSum_v2-6", 
                   st = "10a_iliSum_v1-2")

#### import data ####################################
aggDat <- model11a_iliSum_v7(path_list, mod_list) %>%
    rename(aggbias = y)
modDat <- model8a_iliSum_v7(path_list) %>%
    mutate(seasIntensityRR = y1-logE)

#### plot formatting ####################################
w <- 9; h <- 6

#### plot distribution of dbMetrics ####################################
print(sprintf('plotting db metrics %s', code.str))
dir.create(sprintf('../graph_outputs/EDA_IMS_burden%s', spatial$stringabbr), showWarnings = FALSE)
setwd(sprintf('../graph_outputs/EDA_IMS_burden%s', spatial$stringabbr))

# total seasonal intensity plot
plt.distr.iliSum <- ggplot(modDat, aes(x=seasIntensityRR, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=10) + geom_density() +
  # coord_cartesian(xlim=c(0, 250)) +
  facet_wrap(~season) + ggtitle("Burden: county seasonal intensity relative risk")
ggsave(sprintf("distr_ILITotRR_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.iliSum, width=w, height=h)

# total aggregation bias ILI plot
plt.distr.aggbias <- ggplot(aggDat, aes(x=aggbias, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=10) + geom_density() +
  # coord_cartesian(xlim=c(0, 250)) +
  facet_wrap(~season) + ggtitle("Aggregation bias: seasonal intensity")
ggsave(sprintf("distr_aggbiasILITotRR_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.aggbias, width=w, height=h)


print('finished plotting db metrics')


