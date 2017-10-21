# 9/28/17
# Function: Plot response data in choropleths - by season and average across seasons

require(tidyverse)
require(data.table)

setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
####################################

dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
w <- 5.5; h = 4

setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
path_latlon_reg <- paste0(getwd(), "/region_latlon.csv")
path_region_cw <- paste0(getwd(), "/state_abbreviations_FIPS_region.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_fullIndic_st <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_response_reg <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))
path_fullIndic_reg <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_reg.csv", dbCodeStr))


path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_region_cw = path_region_cw,
                  path_response_cty = path_response_cty,
                  path_fullIndic_cty = path_fullIndic_cty, 
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st,
                  path_fullIndic_st = path_fullIndic_st, 
                  path_latlon_reg = path_latlon_reg,
                  path_response_reg = path_response_reg,
                  path_fullIndic_reg = path_fullIndic_reg)
offset <- FALSE

setwd("../graph_outputs/response_data_explore")
exportPath <- getwd()

# import cty & st data
wksToEpi_ctySt <- import_obs_wksToEpi_ctySt(offset, path_list)
wksToPeak_ctySt <- import_obs_wksToPeak_ctySt(offset, path_list)
iliEarly_ctySt <- import_obs_iliEarly_ctySt(offset, path_list)
iliPeak_ctySt <- import_obs_iliPeak_ctySt(offset, path_list)
# import cty & reg data (so it's plottable on a cty map)
wksToEpi_ctyReg <- import_obs_wksToEpi_ctyReg(offset, path_list)
wksToPeak_ctyReg <- import_obs_wksToPeak_ctyReg(offset, path_list)
iliEarly_ctyReg <- import_obs_iliEarly_ctyReg(offset, path_list)
iliPeak_ctyReg <- import_obs_iliPeak_ctyReg(offset, path_list)

# merge data for all three scales
wksToEpi <- merge_obs_ctyStReg(wksToEpi_ctySt, wksToEpi_ctyReg)
wksToPeak <- merge_obs_ctyStReg(wksToPeak_ctySt, wksToPeak_ctyReg)
iliEarly <- merge_obs_ctyStReg(iliEarly_ctySt, iliEarly_ctyReg)
iliPeak <- merge_obs_ctyStReg(iliPeak_ctySt, iliPeak_ctyReg)


################################
choro_obs_db_oneSeason <- function(prepDat, pltFormats){
  # plot single season choropleth for disease burden measure

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  offset_l <- pltFormats$offset_l
  measure <- pltFormats$measure
  dataProcess <- pltFormats$dataProcess
  dataScale <- pltFormats$dataScale

  names(prepDat) <- gsub(paste0("y_", dataScale), "y", names(prepDat)) 

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
    exportFname <- paste0(exportPath, "/choro_obs_", dataProcess, "_", measure, "_", dataScale, "_S", x, ".png")
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
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  offset_l <- pltFormats$offset_l
  measure <- pltFormats$measure
  dataProcess <- pltFormats$dataProcess
  dataScale <- pltFormats$dataScale

  names(inDat) <- gsub(paste0("y_", dataScale), "y", names(inDat)) 

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
  pltDat <- prepDat2 %>%
    select(fips, Observed) %>%
    gather(fig, bin, Observed) %>%
    filter(fig == "Observed") %>%
    mutate(fig = factor(fig, levels = c("Observed"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(pltDat$bin))
 
  exportFname <- paste0(exportPath, "/choro_obs_", dataProcess, "_", measure, "_", dataScale, "_avg.png")

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

## county data figures ##
format_wksToEpi <- list(w = w, h = h, measure = "wksToEpi", dataProcess = "irDt", legendStep = 5, offset_l = FALSE, dataScale = "cty")
do.call(choro_obs_db_oneSeason, list(wksToEpi, format_wksToEpi))
do.call(choro_obs_db_avgSeason, list(wksToEpi, format_wksToEpi))

format_wksToPeak <- list(w = w, h = h, measure = "wksToPeak", dataProcess = "irDt", legendStep = 4, offset_l = FALSE, dataScale = "cty")
do.call(choro_obs_db_oneSeason, list(wksToPeak, format_wksToPeak))
do.call(choro_obs_db_avgSeason, list(wksToPeak, format_wksToPeak))

format_iliEarly <- list(w = w, h = h, measure = "iliEarly", dataProcess = "irDt", legendStep = 0.5, offset_l = FALSE, dataScale = "cty")
do.call(choro_obs_db_oneSeason, list(iliEarly, format_iliEarly))
do.call(choro_obs_db_avgSeason, list(iliEarly, format_iliEarly))

format_iliPeak <- list(w = w, h = h, measure = "iliPeak", dataProcess = "irDt", legendStep = 0.5, offset_l = FALSE, dataScale = "cty")
do.call(choro_obs_db_oneSEason, list(iliPeak, format_iliPeak))
do.call(choro_obs_db_avgSeason, list(iliPeak, format_iliPeak))

## state data figures ##
format_wksToEpi <- list(w = w, h = h, measure = "wksToEpi", dataProcess = "irDt", legendStep = 5, offset_l = FALSE, dataScale = "st")
do.call(choro_obs_db_oneSeason, list(wksToEpi, format_wksToEpi))
do.call(choro_obs_db_avgSeason, list(wksToEpi, format_wksToEpi))

format_wksToPeak <- list(w = w, h = h, measure = "wksToPeak", dataProcess = "irDt", legendStep = 4, offset_l = FALSE, dataScale = "st")
do.call(choro_obs_db_oneSeason, list(wksToPeak, format_wksToPeak))
do.call(choro_obs_db_avgSeason, list(wksToPeak, format_wksToPeak))

format_iliEarly <- list(w = w, h = h, measure = "iliEarly", dataProcess = "irDt", legendStep = 50, offset_l = FALSE, dataScale = "st")
do.call(choro_obs_db_oneSeason, list(iliEarly, format_iliEarly))
do.call(choro_obs_db_avgSeason, list(iliEarly, format_iliEarly))

format_iliPeak <- list(w = w, h = h, measure = "iliPeak", dataProcess = "irDt", legendStep = 50, offset_l = FALSE, dataScale = "st")
do.call(choro_obs_db_oneSeason, list(iliPeak, format_iliPeak))
do.call(choro_obs_db_avgSeason, list(iliPeak, format_iliPeak))

## region data figures ##
format_wksToEpi <- list(w = w, h = h, measure = "wksToEpi", dataProcess = "irDt", legendStep = 5, offset_l = FALSE, dataScale = "reg")
do.call(choro_obs_db_oneSeason, list(wksToEpi, format_wksToEpi))
do.call(choro_obs_db_avgSeason, list(wksToEpi, format_wksToEpi))

format_wksToPeak <- list(w = w, h = h, measure = "wksToPeak", dataProcess = "irDt", legendStep = 4, offset_l = FALSE, dataScale = "reg")
do.call(choro_obs_db_oneSeason, list(wksToPeak, format_wksToPeak))
do.call(choro_obs_db_avgSeason, list(wksToPeak, format_wksToPeak))

format_iliEarly <- list(w = w, h = h, measure = "iliEarly", dataProcess = "irDt", legendStep = 250, offset_l = FALSE, dataScale = "reg")
do.call(choro_obs_db_oneSeason, list(iliEarly, format_iliEarly))
do.call(choro_obs_db_avgSeason, list(iliEarly, format_iliEarly))

format_iliPeak <- list(w = w, h = h, measure = "iliPeak", dataProcess = "irDt", legendStep = 500, offset_l = FALSE, dataScale = "reg")
do.call(choro_obs_db_oneSeason, list(iliPeak, format_iliPeak))
do.call(choro_obs_db_avgSeason, list(iliPeak, format_iliPeak))
