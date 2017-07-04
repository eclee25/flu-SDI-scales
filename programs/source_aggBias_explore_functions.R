
## Name: Elizabeth Lee
## Date: 6/23/17
## Function: functions to analyze aggregation bias (state and county surveillance models)
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")

#### data import functions ################################
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


#### plotting functions ################################
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