require(tidyverse)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_model_data.R")
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
    legendStep <- 1
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
      scale_fill_brewer(name = "Weeks from Week 40", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") + 
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
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # set breaks based on distribution of observed data
  allObs <- c(obsAllMeasuresDat$rr_iliEarly, obsAllMeasuresDat$rr_iliPeak)
  breaks <- seq(floor(min(allObs, na.rm = TRUE)), ceiling(max(allObs, na.rm = TRUE)), by = legendStep)

  prepDat <- obsAllMeasuresDat %>%
    select(season, fips, contains("rr_")) %>%
     mutate(iliEarly = cut(rr_iliEarly, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(iliPeak = cut(rr_iliPeak, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
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
      scale_fill_brewer(name = "ILI Intensity", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") + 
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
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # set breaks based on distribution of observed data
  allObs <- c(obsBiasAllMeasuresDat$rr_iliEarly, obsBiasAllMeasuresDat$rr_iliPeak)
  breaks <- seq(floor(min(allObs, na.rm = TRUE)), ceiling(max(allObs, na.rm = TRUE)), by = legendStep)

  prepDat <- obsAllMeasuresDat %>%
    select(season, fips, contains("rr_")) %>%
     mutate(iliEarly = cut(rr_iliEarly, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(iliPeak = cut(rr_iliPeak, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat$iliEarly)
  
  plotDat <- prepDat %>%
    select(season, fips, iliEarly, iliPeak) %>%
    gather(fig, bin, iliEarly:iliPeak) %>%
    mutate(fig = factor(fig, levels = c("iliEarly", "iliPeak"), labels = c("Early Season", "Peak Season"))) %>%
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
      scale_fill_brewer(name = "ILI Intensity", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") + 
      facet_wrap(~fig)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################