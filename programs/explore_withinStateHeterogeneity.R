
## Name: Elizabeth Lee
## Date: 8/11/17
## Function: State-specific measures to see which states are more heterogeneous than others (variance in timing); Later, can use this to compare the variance within states (range of values or variance rank) to the aggregation effect

## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse); require(DBI); require(RMySQL) # clean_data_functions dependencies

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)

setwd("../graph_outputs/explore_withinStateHeterogeneity")
path_exportFig <- getwd()

#### Functions #################################
scatter_variance <- function(prepData){
  # filter the data for a single season if needed
  print(match.call())
  
  pltData <- prepData %>%
    mutate(xAxis = factor(hetRank, labels = st))
  
  plt <- ggplot(pltData, aes(x = xAxis, y = variance)) +
    geom_point() +
    scale_y_continuous("Variance in epidemic onset") +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=10), text = element_text(size = 10))
  
  ggsave(paste0(path_exportFig, "/scatter_variance.jpeg"), height = 4, width = 6, units = "in")
}
#################################
boxplot_response <- function(respData){
  # for all seasons
  print(match.call())

  pltData <- respData #%>%
    # mutate(xAxis = factor(hetRank, labels = st))
  
  plt <- ggplot(pltData, aes(st, y = y1)) +
    geom_boxplot() +
    # scale_x_continuous("", labels = )
    scale_y_continuous("Weeks to epidemic onset") +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=10), text = element_text(size = 10)) + 
    facet_grid(season~.)
  
  ggsave(paste0(path_exportFig, "/boxplot_response.jpeg"), height = 8, width = 8, units = "in")
}
#################################
boxplot_response_oneSeas <- function(respData){
  # for all seasons
  print(match.call())

  seasons <- respData %>% distinct(season) %>% unlist
  for (s in seasons){
    seasData <- respData %>% filter(season == s & !is.na(hetRank))
    labelsDf <- seasData %>%
      distinct(hetRank, st) %>% 
      arrange(hetRank)
    
    pltData <- seasData %>%
        mutate(xAxis = factor(hetRank, levels = labelsDf$hetRank, labels = labelsDf$st))
  
    plt <- ggplot(pltData, aes(xAxis, y = y1)) +
        geom_boxplot() +
        ggtitle(paste("Season", s)) +
        scale_y_continuous("Weeks to epidemic onset") +
        theme_bw() + 
        theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=10), text = element_text(size = 10))

    ggsave(paste0(path_exportFig, "/boxplot_response_S", s, ".jpeg"), height = 4, width = 6, units = "in")
  }
  
}

#### MAIN #################################
yDat <- cleanR_wksToEpi_cty(path_list)

# clean data in 2 ways: pooled across seasons, or not
varDat_pooled <- yDat %>%
  group_by(st) %>%
  mutate(forCount = ifelse(!is.na(y1), 1, 0)) %>%
  summarise(variance = var(y1, na.rm = TRUE), counted = sum(forCount), totCty = length(y1)) %>%
  arrange(desc(variance)) %>%
  mutate(hetRank = seq_along(variance))

varDat_bySeas <- yDat %>%
  mutate(forCount = ifelse(!is.na(y1), 1, 0)) %>%
  group_by(st, season) %>%
  summarise(variance = var(y1, na.rm = TRUE), counted = sum(forCount), totCty = length(y1)) %>%
  filter(st != "DC" & counted > 0) %>%
  arrange(season, desc(variance)) %>%
  group_by(season) %>%
  mutate(hetRank = seq_along(variance)) %>%
  ungroup

# link ranks and full response data
respDat <- left_join(yDat, varDat_bySeas %>% select(st, season, hetRank), by = c("season", "st"))

#### plot data ####
# plot variance across states

scatter_variance(varDat_pooled)
boxplot_response(respDat)
boxplot_response_oneSeas(respDat)
