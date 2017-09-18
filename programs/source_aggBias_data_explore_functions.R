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

#### scatter plotting functions ################################
scatter_obsCompare_stCty_wksToEpi <- function(modCodeStr_cty, modCodeStr_st, pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obsFit_wksToEpi_ctySt(modCodeStr_cty, modCodeStr_st, offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_stCty_wksToEpi.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("Observed Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_regCty_wksToEpi <- function(modCodeStr_cty, pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and region data
  plotDat <- import_obs_wksToEpi_ctyReg(modCodeStr_cty, offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_regCty_wksToEpi.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("Observed Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_stCty_wksToPeak <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obs_wksToPeak_ctySt(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_stCty_wksToPeak.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Peak (county)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Peak (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Weeks to Epidemic Peak (county)") +
      scale_y_continuous("Observed Weeks to Epidemic Peak (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_regCty_wksToPeak <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and region data
  plotDat <- import_obs_wksToPeak_ctyReg(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_regCty_wksToPeak.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Peak (county)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Peak (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Weeks to Epidemic Peak (county)") +
      scale_y_continuous("Observed Weeks to Epidemic Peak (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_stCty_iliEarly <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obs_iliEarly_ctySt(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_stCty_iliEarly.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Early Seasonal Intensity (county)") +
      scale_y_continuous("RR for Observed Early Seasonal Intensity (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Early Seasonal Intensity (county)") +
      scale_y_continuous("Observed Early Seasonal Intensity (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_regCty_iliEarly <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obs_iliEarly_ctyReg(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_regCty_iliEarly.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Early Seasonal Intensity (county)") +
      scale_y_continuous("RR for Observed Early Seasonal Intensity (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Early Seasonal Intensity (county)") +
      scale_y_continuous("Observed Early Seasonal Intensity (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_stCty_iliPeak <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obs_iliPeak_ctySt(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_stCty_iliPeak.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Peak Seasonal Intensity (county)") +
      scale_y_continuous("RR for Observed Peak Seasonal Intensity (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Peak Seasonal Intensity (county)") +
      scale_y_continuous("Observed Peak Seasonal Intensity (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################
scatter_obsCompare_regCty_iliPeak <- function(pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obs_iliPeak_ctyReg(offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_data_folder(), "scatter_obsCompare_regCty_iliPeak.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = obs_rr_cty, y = obs_rr_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Observed Peak Seasonal Intensity (county)") +
      scale_y_continuous("RR for Observed Peak Seasonal Intensity (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_y_cty, y = obs_y_reg)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Observed Early Seasonal Intensity (county)") +
      scale_y_continuous("Observed Early Seasonal Intensity (region)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)
    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)
}
################################


#### aggBias plotting functions ################################
choro_obs_aggBias_stCty_wksToEpi_oneSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and state weeks to epi
    prepDat <- import_obsFit_wksToEpi_ctySt(modCodeStr_cty, modCodeStr_st, offset_l, filepathList)

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
choro_obs_aggBias_regCty_wksToEpi_oneSeason <- function(modCodeStr_cty, pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and region weeks to epi
    prepDat <- import_obs_wksToEpi_ctyReg(modCodeStr_cty, offset_l, filepathList)

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
choro_obs_aggBias_stCty_wksToPeak_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and state weeks to peak
    prepDat <- import_obs_wksToPeak_ctySt(offset_l, filepathList)

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
choro_obs_aggBias_regCty_wksToPeak_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and region weeks to peak
    prepDat <- import_obs_wksToPeak_ctyReg(offset_l, filepathList)

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
choro_obs_aggBias_stCty_iliEarly_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and state ili in early flu season
    prepDat <- import_obs_iliEarly_ctySt(offset_l, filepathList)

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
choro_obs_aggBias_regCty_iliEarly_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and region ili in early flu season
    prepDat <- import_obs_iliEarly_ctyReg(offset_l, filepathList)

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
choro_obs_aggBias_stCty_iliPeak_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and state peak ILI
    prepDat <- import_obs_iliPeak_ctySt(offset_l, filepathList)

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
choro_obs_aggBias_regCty_iliPeak_oneSeason <- function(pltFormats, datFormats, filepathList){
    print(match.call())

    # data formatting
    offset_l <- datFormats$offset_l

    # import difference between county and region peak ILI
    prepDat <- import_obs_iliPeak_ctyReg(offset_l, filepathList)

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