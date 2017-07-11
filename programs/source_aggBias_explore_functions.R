
## Name: Elizabeth Lee
## Date: 6/23/17
## Function: functions to analyze aggregation bias (state and county surveillance models)
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
require(lazyeval)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")

#### data import functions ################################
################################


#### plotting functions ################################
################################

choro_fitOverlap_stCty_wksToEpi_oneSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, datFormats, path_list){
  print(match.call())
    
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  
  # data formatting
  offset_l <- datFormats$offset_l

  # import county and state data
  fullDf <- import_obsFit_wksToEpi_ctySt(modCodeStr_cty, modCodeStr_st, offset_l, path_list)

  plotDat <- do.call(overlapping_intervals, list(df=fullDf, intervalA_LB="cty_LB", intervalA_UB="cty_UB", intervalB_LB="st_LB", intervalB_UB="st_UB"))

  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){

    exportFname <- paste0(string_exportFig_aggBias_folder(), "choro_fitOverlap_stCty_wksToEpi_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = overlap, map_id = fips), color = "grey50", size = 0.025) +
      scale_fill_manual(name = "", values = c("1" = "grey75", "0" = "#7b3294"), breaks = c("1", "0"), labels = c("overlap", paste0("no overlap"))) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 14), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") 
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  }
  
}
################################

scatter_fitCompare_stCty_wksToEpi <- function(modCodeStr_cty, modCodeStr_st, pltFormats, datFormats, path_list){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  offset_l <- datFormats$offset_l
  
  # import county and state data
  plotDat <- import_obsFit_wksToEpi_ctySt(modCodeStr_cty, modCodeStr_st, offset_l, path_list) %>%
    mutate(season = as.character(season))

  exportFname <- paste0(string_exportFig_aggBias_folder(), "scatter_fitCompare_stCty_wksToEpi.png")

  # scatterplot with offset
  if(offset_l){
    scatter <- ggplot(plotDat, aes(x = fit_rr_cty, y = fit_rr_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("RR for Fitted Weeks to Epidemic Onset (county)") +
      scale_y_continuous("RR for Fitted Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    } else {
    scatter <- ggplot(plotDat, aes(x = fit_y_cty, y = fit_y_st)) +
      geom_point(colour = "blue", alpha = 0.3) +
      geom_abline(colour = "black", intercept = 0, slope = 1) +
      scale_x_continuous("Fitted Weeks to Epidemic Onset (county)") +
      scale_y_continuous("Fitted Weeks to Epidemic Onset (state)") +
      theme_bw() +
      theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "bottom") +
      facet_wrap(~season, nrow = 2)

    }

  ggsave(exportFname, scatter, height = h, width = w, dpi = dp)

}
################################

#### data processing functions ################################
################################
overlapping_intervals <- function(df, intervalA_LB, intervalA_UB, intervalB_LB, intervalB_UB){
  # primarily for choropleths indicating significance

  df %>%
    mutate_(overlap = interp(~ifelse((aLB <= bLB & bLB <= aUB) | (aLB <= bUB & bUB <= aUB) | (bLB <= aLB & aLB <= bUB) | (bLB <= aUB & aUB <= bUB), "1", "0"), aLB = as.name(intervalA_LB), aUB = as.name(intervalA_UB), bLB = as.name(intervalB_LB), bUB = as.name(intervalB_UB)))
                             
}


#### obsolete functions? ################################
################################
import_fit_aggBias_seasIntensityRR <- function(modCodeStr_cty, modCodeStr_st, filepathList){
  print(match.call())
  # import fitted values for county and state seasonal intensity models

  # import county data
  ctyDat <- import_obsFit_seasIntensityRR(modCodeStr_cty, filepathList) %>%
    rename(fit_rr_cty = fit_rr, fit_logy_cty = fit_logy) %>%
    select(season, fips, fit_rr_cty, fit_logy_cty) %>%
    mutate(fips_st = substring(fips, 1, 2))
  
  # import state data
  stDat <- import_obsFit_seasIntensityRR_st(modCodeStr_st, filepathList) %>%
    rename(fit_rr_st = fit_rr, fit_logy_st = fit_logy) %>%
    select(season, fips_st, fit_rr_st, fit_logy_st)
  
  fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
    mutate(fit_rrDiff_stCty = fit_rr_st-fit_rr_cty) %>%
    mutate(fit_logyRatio_stCty = fit_logy_st-fit_logy_cty) %>%
    select(season, fips, fips_st, fit_rr_cty, fit_logy_cty, fit_rr_st, fit_logy_st, fit_rrDiff_stCty, fit_logyRatio_stCty)
  
  return(fullFitDat) 
}
################################

choro_fit_aggBias_seasIntensityRR_oneSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, filepathList){
  print(match.call())
  # choropleth of error between county and state-level seasonal intensity

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }

  # import error between county and state seasonal intensity RR
  prepDat <- import_fit_aggBias_seasIntensityRR(modCodeStr_cty, modCodeStr_st, filepathList)

  # set breaks based on distribution of log ratio of relative risks from county to state data (aka. error)
  # breaks <- seq(floor(min(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), ceiling(max(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), by = legendStep)
  breaks <- seq(-2.5,3.5,by=1)
  manualPalette <- c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a")

  plotDat <- prepDat %>%
    mutate(Fit_rrDiff = cut(fit_rrDiff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  factorlvls <- levels(plotDat$fit_rrDiff_stCty)

  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_exportFig_subsampleAggBias_folder(), "choro_fit_aggBias_seasIntensityRR_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = Fit_rrDiff, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_manual(name = "Error", values = manualPalette, na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }

}
################################
