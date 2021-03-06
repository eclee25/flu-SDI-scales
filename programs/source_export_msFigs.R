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
################################
string_corrFactor_auc_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/correct_aggBias/"))
}

#### cleaning functions ################################
measure_labels <- function(){
  measureLabelsDf <- data.frame(
    measure = c("iliEarly", "iliPeak", "wksToEpi", "wksToPeak"), 
    measureLab = c("Onset Intensity", "Peak Intensity", "Onset Timing", "Peak Timing"),
    measureType = c(rep("Intensity", 2), rep("Timing", 2)),
    measureTiming = rep(c("Onset", "Peak"), 2)) %>%
    mutate(measureType = factor(measureType, levels = c("Timing", "Intensity"))) %>%
    mutate(measureTiming = factor(measureTiming, levels = c("Onset", "Peak"))) 

  return(measureLabelsDf)
}
################################
season_labels <- function(){
  seasonLabelsDf <- data.frame(season = 3:9, seasLabs = c("2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09"), stringsAsFactors = FALSE)
  return(seasonLabelsDf)
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
    mutate(fig = factor(fig, levels = c("iliEarly", "iliPeak"), labels = c("Onset Intensity", "Peak Intensity"))) %>%
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
    mutate(fig = factor(fig, levels = c("wksToEpi", "wksToPeak", "iliEarly", "iliPeak"), labels = c("Onset Timing", "Peak Timing", "Onset Intensity", "Peak Intensity"))) %>%
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
      scale_fill_manual(name = "Spatial Aggregation Error", values = manualPalette, na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
      facet_wrap(~fig, nrow = 1)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################
correlog_obs_allMeasures <- function(correlogDat, pltFormats){
  print(match.call())
  # correlograms with two panels, one for timing and one for intensity measures
  # 11/3 Could add more resamples and indicators for statistical significance

  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  dataScale <- pltFormats$dataScale
  exportFname <- paste0(string_msFig_folder(), "correlog_obs_allMeasures_", dataScale, ".png")

  correlogPlot <- ggplot(correlogDat %>% filter(pValue < .01), 
    aes(x = meanOfClass, y = correlation)) +
    geom_point(aes(colour = measureTiming), alpha = 0.35) +
    geom_hline(yintercept = 0) +
    geom_vline(aes(xintercept = xIntercept, colour = measureTiming)) +
    scale_colour_tableau() +
    scale_x_continuous("Mean Distance within Class (km)") +
    scale_y_continuous("Correlation") +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_blank()) +
    facet_wrap(~measureType)
  
  ggsave(exportFname, correlogPlot, width = w, height = h, dpi = dp)

}
################################
line_corrFactor_auc <- function(pltFormats){
  print(match.call())
  # compare corrFactor ROC curve to replicates with random corrFactors

  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  # import data
  fnameLs <- list.files(string_corrFactor_auc_folder(), full.names = TRUE)
  inDat <- map_df(fnameLs, function(x){
    return(read_csv(x))
  })

  corrFactorDat <- inDat %>%
    mutate(measure = unlist(tstrsplit(modCodeStr, "_", keep = 2))) %>%
    left_join(measure_labels(), by = "measure") %>%
    mutate(colourlab = ifelse(grepl("rnorm", coefName), coefName, "corrFactor"))

  # plot formatting
  exportFname <- paste0(string_msFig_folder(), "line_corrFactor_auc_cty.png")
  colorFormats <- corrFactorDat %>%
    distinct(colourlab) %>%
    mutate(colourname = ifelse(colourlab == "corrFactor", "#800000", "grey50"))
  
  # plot data
  plt <- ggplot(corrFactorDat, aes(x = fpr, y = sensitivity)) +
    geom_line(aes(colour = colourlab), alpha = 0.8, size = 1) +
    geom_point(aes(colour = colourlab)) +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = 2) +
    scale_colour_manual(values = colorFormats$colourname, breaks = colorFormats$colourlab) +
    scale_x_continuous("False Positive Rate (1-Specificity)") +
    scale_y_continuous("Sensitivity") +
    theme_bw() +
    theme(text = element_text(size = 12)) +
    guides(colour = "none") +
    facet_wrap(~measureLab)
  ggsave(exportFname, plt, width = w, height = h, dpi = dp)
}