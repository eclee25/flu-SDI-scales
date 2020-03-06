
## Name: Elizabeth Lee
## Date: 8/14/17
## Function: Plot choropleths for all centered and standardized predictors
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI)#; require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
#require(INLA) # main dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- c(3:9)
origin_locations_file <- "Lee"


#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaDiagnostics.R")

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

setwd("./origin_locations")
path_srcLoc_cty <- paste0(getwd(), sprintf("/fluseason_source_locations_%s.csv", origin_locations_file))


# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty, 
                  path_graphIdx_cty = path_graphIdx_cty,
                  path_graphExport_st = path_graphExport_st,
                  path_graphIdx_st = path_graphIdx_st,
                  path_fullIndic_cty = path_fullIndic_cty,
                  path_srcLoc_cty = path_srcLoc_cty)

#### Import and process data ####
modData <- model8f_wksToEpi_v7(path_list)

#### export formatting ####
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_choro_inlaPredictors", showWarnings = FALSE)
setwd("../graph_outputs/EDA_choro_inlaPredictors")
path_plotExport <- getwd()

#### clean data for plotting #####
modData2 <- modData %>%
  select(season, fips, contains("X_"), contains("O_")) %>%
  gather(RV, predictor, -season, -fips) %>%
  clean_RVnames(.) 
modData3 <- modData2 %>%
  spread(RV, predictor)

varnames <- modData2 %>% distinct(RV) %>% unlist

#### plot functions #####
plot_countyChoro_predictors <- function(exportPath, pltDat, pltVarTxt, code, zeroes){
  # draw state choropleth with tiers or gradient colors and export to file
  print(match.call())

  countyMap <- map_data("county")
  data(county.fips)
  # plot formatting
  h <- 5; w <- 8; dp <- 300

  # merge county data
  polynameSplit <- tstrsplit(county.fips$polyname, ",")
  ctyMap <- tbl_df(county.fips) %>%
    mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
    mutate(region = polynameSplit[[1]]) %>%
    mutate(subregion = polynameSplit[[2]]) %>%
    full_join(countyMap, by = c("region", "subregion")) %>%
    filter(!is.na(polyname) & !is.na(long)) %>%
    rename(state = region, county = subregion) %>%
    rename(region = fips) %>%
    select(-polyname)

  # tier choropleth
  if (code == 'tier'){
    # process data for tiers
    # 7/21/16: natural breaks w/ classIntervals
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt)
    # create natural break intervals with jenks algorithm
    intervals <- classIntervals(pltDat$pltVar[!is.na(pltDat$pltVar)], n = 5, style = "jenks")
    if (zeroes){
      # 0s have their own color
      if (0 %in% intervals$brks){
        breakList <- intervals$brks
      } else {
        breakList <- c(0, intervals$brks)
      }
      breaks <- sort(breakList)
    } else{
      breaks <- c(intervals$brks)
    }
    breaksRound <- round(breaks, 1)
    breakLabels <- matrix(1:(length(breaksRound)-1))
    for (i in 1:length(breakLabels)){
      # create legend labels
      breakLabels[i] <- paste0("(",as.character(breaksRound[i]), "-", as.character(breaksRound[i+1]), "]")}
    # reverse order of break labels so zeros are green and larger values are red
    breakLabels <- rev(breakLabels)
    pltDat2 <- pltDat %>%
      mutate(pltVarBin = factor(.bincode(pltVar, breaks, right = TRUE, include.lowest = TRUE))) %>%
      mutate(pltVarBin = factor(pltVarBin, levels = rev(levels(pltVarBin))))


    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat2, map = ctyMap, aes(fill = pltVarBin, map_id = fips), color = "grey25", size = 0.15) +
      scale_fill_brewer(name = pltVarTxt, palette = "RdYlGn", label = breakLabels, na.value = "grey60") +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
      facet_wrap(~season)
  }

  # gradient choropleth
  else if (code == 'gradient'){
    # data for gradient has minimal processing
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt)

    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id=region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = pltVar, map_id = fips), color = "grey25", size = 0.15) +
      scale_fill_continuous(name = pltVarTxt, low = "#f0fff0", high = "#006400") +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
      facet_wrap(~season)
  }

  ggsave(exportPath, choro, height = h, width = w, dpi = dp)

}

#### Diagnostic plots ################################
# for (v in varnames){
#   path_choroDummy <- paste0(path_plotExport, "/choro_", v, "_allSeas_grad.png")
#   plot_countyChoro_predictors(path_choroDummy, modData3, v, "gradient", FALSE)
# }

# for (v in varnames[c(13, 14, 17, 18)]){
#   for (s in seasons){
#     subData <- modData3 %>%
#       select_("season", "fips", v) %>%
#       filter(season == s)
#     path_choroDummy_1seas <- paste0(path_plotExport, "/choro_", v, "_S", s, "_grad.png")
#     plot_countyChoro(path_choroDummy_1seas, subData, v, "gradient", FALSE)
#   }
# }

#### Quantify variation of predictors ################################
varDf <- modData2 %>%
  filter(RV != "graphIdx_st") %>%
  mutate(season = paste0("S", season)) %>%
  group_by(RV, season) %>%
  summarise(variance = var(predictor, na.rm = TRUE)) %>%
  spread(season, variance) %>%
  arrange(S3)

varDf2 <- modData2 %>%
  filter(RV != "graphIdx_st") %>%
  mutate(season = paste0("S", season)) %>%
  group_by(RV) %>%
  summarise(variance = var(predictor, na.rm = TRUE)) %>%
  arrange(variance)
