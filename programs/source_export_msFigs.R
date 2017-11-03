require(tidyverse)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
################################################################

#### filepath functions ################################
string_msFig_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/msFigs/"))
}
################################
string_refData_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../reference_data/"))
}

#### plotting functions ################################
choro_obs_timingMeasures_oneSeason <- function(obsAllMeasuresDat, pltFormats){
  # plot choropleths for observed timing measures by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 4
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # set breaks based on distribution of observed data
  allObs <- c(obsAllMeasuresDat$y_wksToEpi, obsAllMeasuresDat$y_wksToPeak)
  breaks <- seq(floor(min(allObs, na.rm = TRUE)), ceiling(max(allObs, na.rm = TRUE)), by = legendStep)

  prepDat <- obsAllMeasuresDat %>%
    select(season, fips, contains("y_")) %>%
     mutate(wksToEpi = cut(y_wksToEpi, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(wksToPeak = cut(y_wksToPeak, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat$wksToEpi)
  
  plotDat <- prepDat %>%
    select(season, fips, wksToEpi, wksToPeak) %>%
    gather(fig, bin, wksToEpi:wksToPeak) %>%
    mutate(fig = factor(fig, levels = c("wksToEpi", "wksToPeak"), labels = c("Onset Timing", "Peak Timing"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_obs_timingMeasures_cty_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Weeks from\nWeek 40", palette = "RdPu", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
      facet_wrap(~fig)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################
choro_obs_magnitudeMeasures_oneSeason <- function(obsAllMeasuresDat, pltFormats){
  # plot choropleths for observed magnitude measures by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 0.5
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # set breaks based on distribution of observed data
  obsAllMeasuresDat2 <- obsAllMeasuresDat %>%
    mutate(log_rr_iliEarly_shift1 = log(rr_iliEarly+1), log_rr_iliPeak_shift1 = log(rr_iliPeak+1))
  allObs <- c(obsAllMeasuresDat2$log_rr_iliEarly_shift1, obsAllMeasuresDat2$log_rr_iliPeak_shift1)
  breaks <- seq(floor(min(allObs, na.rm = TRUE)), ceiling(max(allObs, na.rm = TRUE)), by = legendStep)

  prepDat <- obsAllMeasuresDat2 %>%
    select(season, fips, contains("rr_")) %>%
    mutate(iliEarly = cut(log_rr_iliEarly_shift1, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(iliPeak = cut(log_rr_iliPeak_shift1, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat$iliEarly)
  
  plotDat <- prepDat %>%
    select(season, fips, iliEarly, iliPeak) %>%
    gather(fig, bin, iliEarly:iliPeak) %>%
    mutate(fig = factor(fig, levels = c("iliEarly", "iliPeak"), labels = c("Early Season", "Peak Season"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_obs_magnitudeMeasures_cty_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Log Intensity", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
      facet_wrap(~fig)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################
choro_obs_aggBias_allMeasures_oneSeason <- function(obsBiasAllMeasuresDat, pltFormats){
  # plot choropleths for observed magnitude measures by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 3 blue - grey - 3 green
  breaks <- c(-22, -10, -5, -1, 1, 5, 10, 22) # set manually

  # clean data
  prepDat <- obsBiasAllMeasuresDat %>%
    select(season, fips, contains("bias_")) %>%
    mutate(wksToEpi = cut(bias_wksToEpi, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(wksToPeak = cut(bias_wksToPeak, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(iliEarly = cut(bias_iliEarly, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(iliPeak = cut(bias_iliPeak, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat$iliEarly)
  
  plotDat <- prepDat %>%
    select(season, fips, wksToEpi, wksToPeak, iliEarly, iliPeak) %>%
    gather(fig, bin, wksToEpi:iliPeak) %>%
    mutate(fig = factor(fig, levels = c("wksToEpi", "wksToPeak", "iliEarly", "iliPeak"), labels = c("Onset Timing", "Peak Timing", "Early Season", "Peak Season"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_obs_aggBias_allMeasures_cty_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()

    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_manual(name = "Aggregation Bias", values = manualPalette, na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
      facet_wrap(~fig, nrow = 1)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################