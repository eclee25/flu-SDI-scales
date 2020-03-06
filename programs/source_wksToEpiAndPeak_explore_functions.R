
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

tsExplore_obs_wksToEpi_cty <- function(path_list, pltFormats){
    # sample random fips-season combos where weeksToEpi > 20 or weeksToEpi < 5 and plot inSeason time series 
    print(match.call())

    sampleSize <- pltFormats$sampleSize
    set.seed(pltFormats$seed)

    # import weeks to epi data
    epiData <- cleanR_wksToEpi_cty(path_list) %>%
        rename(obs_wksToEpi = y1, E_wksToEpi = E) %>%
        select(season, fips, obs_wksToEpi, E_wksToEpi)

    # import full time series data set
    fullIndicData <- read_csv(path_list$path_fullIndic_cty, col_types = cols_only(fips = "c", Thu.week = "D", season = "i", ilin.dt = "d", .fitted = "d", flu.week = "l", in.season = "l")) %>%
        mutate(filterID = paste(fips, season, sep = "S"))

    # LATE EPI: sample fips-season combinations with late epidemic onsets to examine their time series
    sample_lateEpiData <- epiData %>% 
        filter(obs_wksToEpi > 20) %>%
        sample_n(sampleSize) %>%
        mutate(filterID = paste(fips, season, sep = "S")) %>%
        mutate(for.plot = seq_along(1:nrow(.))) %>%
        select(filterID, for.plot)
    pltData_late <- right_join(fullIndicData, sample_lateEpiData, by = c("filterID"))

    # EARLY EPI: sample fips-season combinations with early epidemic onsets to examine their time series
    sample_earlyEpiData <- epiData %>% 
        filter(obs_wksToEpi < 7) %>%
        sample_n(sampleSize) %>%
        mutate(filterID = paste(fips, season, sep = "S")) %>%
        mutate(for.plot = seq_along(1:nrow(.))) %>%
        select(filterID, for.plot)
    pltData_early <- right_join(fullIndicData, sample_earlyEpiData, by = c("filterID")) 

    View(pltData_late)
    View(pltData_late %>% filter(for.plot == 1))
    
    # export inSeason plots for early and late epidemic onset samples
    tsExplore_inSeason_plot(pltData_late, pltFormats, "lateEpi")
    tsExplore_inSeason_plot(pltData_early, pltFormats, "earlyEpi")

}
################################

tsExplore_inSeason_plot <- function(pltData, pltFormats, exploreCode){
    print(match.call())

    numPerPlot <- pltFormats$numPerPlot
    w <- pltFormats$w; h <- pltFormats$h

    # prepare indexes for plotting multiple time series per figure
    indexes <- seq(1, max(pltData$for.plot), by = numPerPlot)
    
    for (i in indexes){
        # in.season plots
        dummyplots <- ggplot(pltData %>% filter(for.plot >= i & for.plot < i+numPerPlot), aes(x = Thu.week, y = ilin.dt)) +
            theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold")) +
            geom_line() + 
            geom_line(aes(color = in.season)) +  
            scale_color_brewer(name = 'in.season', palette = 'Set1') +   
            geom_line(aes(y = .fitted), color = 'grey') + 
            # geom_line(aes(y=ifelse(flu.week, 1, NA)), color = 'black') + 
            facet_wrap(~filterID, scales = "free") 
        
        # grab labels for filename
        labs <- pltData %>% select(filterID) %>% distinct(filterID) %>% slice(c(i, i+numPerPlot-1))
        exportFname <- paste0(string_exportFig_wksToEpiAndPeak_folder(), "tsExplore_obs_wksToEpi_cty_", exploreCode, "_", labs[1,], "-", labs[2,], ".jpg")

        ggsave(exportFname, dummyplots, width = w, height = h)
    }
}
################################