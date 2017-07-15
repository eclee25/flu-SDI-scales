
## Name: Elizabeth Lee
## Date: 7/14/17
## Function: functions to explore (compare) data from weeks to epi and weeks to peak surveillance models at county and state scales
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
require(lazyeval)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")
source("source_clean_response_functions_cty.R")
source("source_clean_response_functions_st.R")

#### plotting functions ################################
################################

scatter_obsCompare_wksToEpiAndPeak_cty <- function(path_list, pltFormats, datFormats){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import weeks to epi data
  epiDat <- cleanR_wksToEpi_cty(path_list) %>%
    rename(obs_wksToEpi = y1, E_wksToEpi = E) %>%
    select(season, fips, obs_wksToEpi, E_wksToEpi)

  # import weeks to peak data
  peakDat <- cleanR_wksToPeak_cty(path_list) %>%
    rename(obs_wksToPeak = y1, E_wksToPeak = E) %>%
    select(season, fips, obs_wksToPeak, E_wksToPeak)

  # merge epi and peak data
  plotDat <- full_join(epiDat, peakDat, by = c("season", "fips"))

  exportFname <- paste0(string_exportFig_wksToEpiAndPeak_folder(), "scatter_obsCompare_wksToEpiAndPeak_cty.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat %>%
      mutate(obs_rr_epi = obs_wksToEpi/E_wksToEpi,
        obs_rr_peak = obs_wksToPeak/E_wksToPeak), aes(x = obs_rr_epi, y = obs_rr_peak)) +
      geom_point(colour = "blue", alpha = 0.3) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Peak (county)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_wksToEpi, y = obs_wksToPeak)) +
      geom_point(colour = "blue", alpha = 0.3) +
      scale_x_continuous("Observed Weeks to Epidemic Onset (county)") +
      scale_y_continuous("Observed Weeks to Epidemic Peak (county)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)

}
################################

scatter_obsCompare_wksToEpiAndPeak_st <- function(path_list, pltFormats, datFormats){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import weeks to epi data
  epiDat <- cleanR_wksToEpi_st(path_list) %>%
    rename(obs_wksToEpi = y1, E_wksToEpi = E) %>%
    select(season, fips_st, obs_wksToEpi, E_wksToEpi)

  # import weeks to peak data
  peakDat <- cleanR_wksToPeak_st(path_list) %>%
    rename(obs_wksToPeak = y1, E_wksToPeak = E) %>%
    select(season, fips_st, obs_wksToPeak, E_wksToPeak)

  # merge epi and peak data
  plotDat <- full_join(epiDat, peakDat, by = c("season", "fips_st"))
  View(plotDat)

  exportFname <- paste0(string_exportFig_wksToEpiAndPeak_folder(), "scatter_obsCompare_wksToEpiAndPeak_st.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat %>%
      mutate(obs_rr_epi = obs_wksToEpi/E_wksToEpi,
        obs_rr_peak = obs_wksToPeak/E_wksToPeak), aes(x = obs_rr_epi, y = obs_rr_peak)) +
      geom_point(colour = "blue", alpha = 0.3) +
      scale_x_continuous("RR for Observed Weeks to Epidemic Onset (state)") +
      scale_y_continuous("RR for Observed Weeks to Epidemic Peak (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = obs_wksToEpi, y = obs_wksToPeak)) +
      geom_point(colour = "blue", alpha = 0.3) +
      scale_x_continuous("Observed Weeks to Epidemic Onset (state)") +
      scale_y_continuous("Observed Weeks to Epidemic Peak (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)

}
################################

