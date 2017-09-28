## Name: Elizabeth Lee
## Date: 9/17/17
## Function: functions to analyze aggregation bias directly from data observations
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
require(lazyeval)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")

#### statistics functions ################################
pairedTest_aggBias_timingMagnitude <- function(obs_early, obs_peak){
  print(match.call())
  # compare aggregation bias for wksToEpi-wksToPeak, iliEarly-iliPeak measures, works for both ctySt and ctyReg levels
  
  # import data
  dat1 <- obs_early %>%
    select(season, fips, fips_st, contains("obs_diff")) 
  names(dat1)[4] <- "earlyBias"

  dat2 <- obs_peak %>%
    select(season, fips, contains("obs_diff")) 
  names(dat2)[3] <- "peakBias"

  fullDat <- full_join(dat1, dat2, by = c("season", "fips")) %>%
    mutate(aggBiasDiff = peakBias - earlyBias) %>%
    mutate(abs_peakBias = abs(peakBias), abs_earlyBias = abs(earlyBias)) %>%
    mutate(absAggBiasDiff = abs_peakBias - abs_earlyBias)
  fullDat2 <- fullDat %>%
    gather(measure, value, earlyBias:absAggBiasDiff)

  # check that the difference between the measures follows a normal distribution, thus making it appropriate for a paired T-test 
  histPlot <- hist(fullDat$aggBiasDiff, bins = 100)
  absHistPlot <- hist(fullDat$absAggBiasDiff, bins = 100)


  # plot overlapping histogram for the two distributions
  dbPlot <- ggplot(fullDat2 %>%
    filter(measure %in% c("earlyBias", "peakBias")), aes(x = value)) +
    geom_freqpoly(aes(y = ..density.., colour = measure), bins = 50, na.rm=TRUE) +
    theme_bw() +
    theme(legend.position = "bottom")
  print(dbPlot)

  absDbPlot <- ggplot(fullDat2 %>%
    filter(measure %in% c("abs_earlyBias", "abs_peakBias")), aes(x = value)) +
    geom_freqpoly(aes(y = ..density.., colour = measure), bins = 50, na.rm=TRUE) +
    theme_bw() +
    theme(legend.position = "bottom")
  print(absDbPlot)

  # perform paired t-test
  ttest <- t.test(fullDat$earlyBias, fullDat$peakBias, paired = TRUE)
  print(ttest)

  absTtest <- t.test(fullDat$abs_earlyBias, fullDat$abs_peakBias, paired = TRUE)
  print(absTtest)

  return(list(histPlot=histPlot, absHistPlot=absHistPlot, dbPlot=dbPlot, absDbPlot=absDbPlot, ttest=ttest, absTtest=absTtest))

}
################################


#### scatter plotting functions ################################
scatter_obsCompare_aggBias_timingMagnitude <- function(obs_measure_aggBias, staticFormats, dynFormats){
  print(match.call())

  # plot formatting
  w <- staticFormats$w; h <- staticFormats$h; dp <- 300
  offset_l <- staticFormats$offset_l
  measure <- dynFormats$measure; bigscale <- dynFormats$bigscale
  
  # import county and state data
  plotDat <- obs_measure_aggBias %>%
    mutate(season = as.character(season)) 

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_", bigscale, "Cty_", measure, ".png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat %>% rename_(obs_bigscale = paste0("obs_rr_", bigscale)), aes(x = obs_rr_cty, y = obs_bigscale)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous(sprintf("RR for Observed %s (county)", measure)) +
      scale_y_continuous(sprintf("RR for Observed %s (%s)", measure, bigscale)) +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat %>% rename_(obs_bigscale = paste0("obs_y_", bigscale)), aes(x = obs_y_cty, y = obs_bigscale)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous(sprintf("Observed %s (county)", measure)) +
      scale_y_continuous(sprintf("Observed %s (%s)", measure, bigscale)) +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################


#### aggBias plotting functions ################################
choro_obs_aggBias_avgSeason <- function(importDat, pltFormats, breaks){
    print(match.call())

    dbCode <- pltFormats$dbCode; scaleDiff <- pltFormats$scaleDiff

    # import aggregation bias
    prepDat <- importDat %>%
      rename_("obs_aggBias" = pltFormats$pltVar) %>%
      group_by(fips) %>%
      summarise(obs_aggBias = mean(obs_aggBias, na.rm = TRUE))

    # # check breaks for aggBias
    # print(hist(prepDat$obs_aggBias))
    # print(summary(prepDat))

    # breaks have 8 values
    pltFormats$breaks <- breaks
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 3 blue - grey - 3 green

    pltDat <- prepDat %>%
      mutate(obs_diff = cut(obs_aggBias, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    
    factorlvls <- levels(pltDat$obs_aggBias)

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), dbCode, "/choro_obs_aggBias_", scaleDiff, "_", dbCode, "_seasonAvg.png")

    choro_aggBias_oneSeason(pltDat, pltFormats)

}
#################################
choro_obs_aggBias_stCty_wksToEpi_oneSeason <- function(obs_wksToEpi_ctySt, pltFormats){
    print(match.call())

    # import difference between county and state weeks to epi
    prepDat <- obs_wksToEpi_ctySt

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_stCty))
    # print(summary(prepDat))

    breaks <- c(-21, -8, -3, -1, 1, 3, 8, 16)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 3 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_stCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "wksToEpi/choro_obs_aggBias_stCty_wksToEpi_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_regCty_wksToEpi_oneSeason <- function(obs_wksToEpi_ctyReg, pltFormats){
    print(match.call())

    # import difference between county and region weeks to epi
    prepDat <- obs_wksToEpi_ctyReg

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_regCty))
    # print(summary(prepDat))

    breaks <- c(-25, -15, -4, -1, 1, 4, 15)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41") # 3 blue - grey - 2 green
    # c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 2 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_regCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_regCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "wksToEpi/choro_obs_aggBias_regCty_wksToEpi_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_stCty_wksToPeak_oneSeason <- function(obs_wksToPeak_ctySt, pltFormats){
    print(match.call())

    # import difference between county and state weeks to peak
    prepDat <- obs_wksToPeak_ctySt

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_stCty))
    # print(summary(prepDat))

    breaks <- c(-20, -8, -4, -1, 1, 4, 8, 18)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 3 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_stCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "wksToPeak/choro_obs_aggBias_stCty_wksToPeak_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_regCty_wksToPeak_oneSeason <- function(obs_wksToPeak_ctyReg, pltFormats){
    print(match.call())

    # import difference between county and region weeks to peak
    prepDat <- obs_wksToPeak_ctyReg

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_regCty))
    # print(summary(prepDat))

    breaks <- c(-18, -8, -4, -1, 1, 4, 8, 20)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 3 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_regCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_regCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "wksToPeak/choro_obs_aggBias_regCty_wksToPeak_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_stCty_iliEarly_oneSeason <- function(obs_iliEarly_ctySt, pltFormats){
    print(match.call())

    # import difference between county and state ili in early flu season
    prepDat <- obs_iliEarly_ctySt

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_stCty))
    # print(summary(prepDat))

    breaks <- c(-80, -15, -5, -1, 1, 5, 25)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41") # 3 blue - grey - 2 green
    # c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 2 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_stCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "iliEarly/choro_obs_aggBias_stCty_iliEarly_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_regCty_iliEarly_oneSeason <- function(obs_iliEarly_ctyReg, pltFormats){
    print(match.call())

    # import difference between county and region ili in early flu season
    prepDat <- obs_iliEarly_ctyReg

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_regCty))
    # print(summary(prepDat))

    breaks <- c(-83, -13, -4, -1, 1, 4, 13)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41") # 3 blue - grey - 2 green
    # c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 2 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_regCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_regCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "iliEarly/choro_obs_aggBias_regCty_iliEarly_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_stCty_iliPeak_oneSeason <- function(obs_iliPeak_ctySt, pltFormats){
    print(match.call())

    # import difference between county and state peak ILI
    prepDat <- obs_iliPeak_ctySt

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_stCty))
    # print(summary(prepDat))

    breaks <- c(-52, -14, -4, -1, 1, 4, 14)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41") # 3 blue - grey - 2 green
    # c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 2 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_stCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "iliPeak/choro_obs_aggBias_stCty_iliPeak_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################
choro_obs_aggBias_regCty_iliPeak_oneSeason <- function(obs_iliPeak_ctyReg, pltFormats){
    print(match.call())

    # import difference between county and region peak ILI
    prepDat <- obs_iliPeak_ctyReg

    # # check breaks for aggBias
    # print(hist(prepDat$obs_diff_regCty))
    # print(summary(prepDat))

    breaks <- c(-60, -10, -4, -1, 1, 4, 10)
    pltFormats$manualPalette <- c("#10456a", "#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41") # 3 blue - grey - 2 green
    # c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a") # 2 blue - grey - 3 green

    plotDat <- prepDat %>%
    mutate(obs_diff = cut(obs_diff_regCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
    factorlvls <- levels(plotDat$obs_diff_regCty)

    seasLs <- plotDat %>% distinct(season) %>% unlist
    for (s in seasLs){

    pltFormats$exportFname <- paste0(string_exportFig_aggBias_data_folder(), "iliPeak/choro_obs_aggBias_regCty_iliPeak_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro_aggBias_oneSeason(pltDat, pltFormats)
    }
}
################################

#### internal plotting functions ################################
choro_aggBias_oneSeason <- function(pltDat, pltFormats){
    print(match.call())

    # assign plot formats
    manualPalette <- pltFormats$manualPalette
    exportFname <- pltFormats$exportFname
    h <- pltFormats$h; w <- pltFormats$w; dp <- 300

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = obs_diff, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_manual(name = "Error", values = manualPalette, na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
}
################################

#### data export functions ################################
write_st_aggBias <- function(importDat, dataFormats){
  print(match.call())

  writeData <- importDat %>%
    rename_("obs_aggBias" = dataFormats$pltVar) %>%
    group_by(fips_st) %>%
    summarise(obs_aggBias = mean(obs_aggBias, na.rm = TRUE)) %>%
    mutate(obs_aggBiasMag = abs(obs_aggBias))

  print(summary(writeData))

  write_csv(writeData, dataFormats$exportPath)
}
#################################
write_cty_aggBias <- function(importDat, dataFormats){
  print(match.call())

  writeData <- importDat %>%
    rename_("obs_aggBias" = dataFormats$pltVar) %>%
    group_by(fips) %>%
    summarise(obs_aggBias = mean(obs_aggBias, na.rm = TRUE)) %>%
    mutate(obs_aggBiasMag = abs(obs_aggBias))

  write_csv(writeData, dataFormats$exportPath)
}
#################################


