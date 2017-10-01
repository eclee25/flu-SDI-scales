# 9/28/17
# Function: Plot response data in choropleths - by season and average across seasons

require(tidyverse)
require(data.table)

setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
####################################

dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
w <- 5.5; h = 4

setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty,
                  path_fullIndic_cty = path_fullIndic_cty)

setwd("../graph_outputs/response_data_explore")
exportPath <- getwd()

wksToEpi <- import_obs_wksToEpi(path_list)
wksToPeak <- import_obs_wksToPeak(path_list)
iliEarly <- import_obs_iliEarly(path_list)
iliPeak <- import_obs_iliPeak(path_list)

################################
choro_obs_db_oneSeason <- function(inDat, pltFormats){
  # plot single season choropleth for disease burden measure
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  offset_l <- pltFormats$offset_l
  measure <- pltFormats$measure

  prepDat <- inDat %>%
    mutate(obs_rr = obs_y/E)

  # set breaks based on distribution of observed data
  if(!offset_l){
    breaks <- seq(floor(min(prepDat$obs_y, na.rm = TRUE)), ceiling(max(prepDat$obs_y, na.rm = TRUE)), by = legendStep)
    prepDat2 <- prepDat %>%
      mutate(Observed = cut(obs_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  } else{
    breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
    prepDat2 <- prepDat %>%
      mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  }
  
  factorlvls <- levels(prepDat2$Observed)
  plotDat <- prepDat2 %>%
    select(season, fips, Observed) %>%
    gather(fig, bin, Observed) %>%
    filter(fig == "Observed") %>%
    mutate(fig = factor(fig, levels = c("Observed"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist

  
  plotChoro <- function(x){
    exportFname <- paste0(exportPath, "/choro_obs_", measure, "_S", x, ".png")
    pltDat <- plotDat %>% filter(season == x)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = paste0("Observed ", measure), palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  } 
  
  purrr::map(seasLs, plotChoro)
     
}
################################
choro_obs_db_avgSeason <- function(inDat, pltFormats){
  # plot choropleth across seasons for disease burden measure
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  offset_l <- pltFormats$offset_l
  measure <- pltFormats$measure

  prepDat <- inDat %>%
    group_by(fips) %>%
    summarise(obs_y = mean(obs_y, na.rm = TRUE), E = mean(E, na.rm = TRUE)) %>%
    mutate(obs_rr = obs_y/E) %>%
    ungroup

  if(!offset_l){
    breaks <- seq(floor(min(prepDat$obs_y, na.rm = TRUE)), ceiling(max(prepDat$obs_y, na.rm = TRUE)), by = legendStep)
    prepDat2 <- prepDat %>%
      mutate(Observed = cut(obs_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  } else{
    breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
    prepDat2 <- prepDat %>%
      mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  }
  
  factorlvls <- levels(prepDat2$Observed)
  plotDat <- prepDat2 %>%
    select(season, fips, Observed) %>%
    gather(fig, bin, Observed) %>%
    filter(fig == "Observed") %>%
    mutate(fig = factor(fig, levels = c("Observed"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  exportFname <- paste0(exportPath, "/choro_obs_", measure, "_avg.png")

  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
    scale_fill_brewer(name = paste0("Observed ", measure), palette = "OrRd", na.value = "grey60", drop = FALSE) +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
}

#### MAIN CODE ################################

# figures by season
format_wksToEpi <- list(w = w, h = h, measure = "wksToEpi", legendStep = 6, offset_l = FALSE)
do.call(choro_obs_db_oneSeason, c(wksToEpi, format_wksToEpi))
do.call(choro_obs_db_avgSeason, c(wksToEpi, format_wksToEpi))

# format_wksToPeak <- list(w = w, h = h, measure = "wksToPeak", legendStep = 6, offset_l = FALSE)
# choro_obs_db_oneSeason(wksToPeak, format_wksToPeak)

# format_iliEarly <- list(w = w, h = h, measure = "iliEarly", legendStep = 10, offset_l = FALSE)
# choro_obs_db_oneSeason(iliEarly, format_iliEarly)

# format_iliPeak <- list(w = w, h = h, measure = "iliPeak", legendStep = 9, offset_l = FALSE)
# choro_obs_db_oneSeason(iliPeak, format_iliPeak)
