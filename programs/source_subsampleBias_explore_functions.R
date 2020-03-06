
## Name: Elizabeth Lee
## Date: 6/23/17
## Function: functions to analyze subsample bias (region models compared to county surveillance model)
## Filenames: 
## Data Source: 
## Notes: 
################################

require(tidyverse)
setwd(dirname(sys.frame(1)$ofile))
source("source_import_modeldata.R")

#### data import functions ################################
################################
import_fit_subsampleBias_seasIntensityRR <- function(modCodeStr_cty, modCodeStr_ss, filepathList){
	print(match.call())
	
	# imported fitted values for county-level seasonal intensity models (full and region subsample)

	# import county data
	ctyDat <- import_obsFit_seasIntensityRR(modCodeStr_cty, filepathList) %>%
		rename(fit_rr_cty = fit_rr, fit_logy_cty = fit_logy) %>%
		select(season, fips, fit_rr_cty, fit_logy_cty) %>%
		mutate(fips_st = substring(fips, 1, 2))

	# import region model data
	regSDat <- import_obsFit_seasIntensityRR(modCodeStr_ss, filepathList) %>%
		rename(fit_rr_ss = fit_rr, fit_logy_ss = fit_logy) %>%
		select(season, fips, fit_rr_ss, fit_logy_ss)
  
	fullFitDat <- right_join(ctyDat, regSDat, by = c("season", "fips")) %>%
		mutate(fit_rrDiff_fullSs = fit_rr_ss-fit_rr_cty) %>%
		mutate(fit_logyRatio_fullSs = fit_logy_ss-fit_logy_cty) %>%
		select(season, fips, fips_st, fit_rr_cty, fit_logy_cty, fit_rr_ss, fit_logy_ss, fit_rrDiff_fullSs, fit_logyRatio_fullSs)
  
  return(fullFitDat) 
}


#### plotting functions ################################
################################
choro_fit_subsampleBias_seasIntensityRR_oneSeason <- function(modCodeStr_cty, modCodeStr_ss, pltFormats, filepathList){
  print(match.call())
  # choropleth of error between county seasonal intensity for full and subsample of data

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  # grab region group text
  regionText <- gsub("8a_iliSum_v2-6_", "", modCodeStr_ss)

  # import error between full and subsample seasonal intensity RR
  prepDat <- import_fit_subsampleBias_seasIntensityRR(modCodeStr_cty, modCodeStr_ss, filepathList)

  # set breaks based on distribution of log ratio of relative risks from county to state data (aka. error)
  # breaks <- seq(floor(min(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), ceiling(max(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), by = legendStep)
  
  breaks <- seq(-2.5,3.5,by=1)
  manualPalette <- c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a")

  plotDat <- prepDat %>%
    mutate(Fit_rrDiff = cut(fit_rrDiff_fullSs, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  factorlvls <- levels(plotDat$fit_rrDiff_fullSs)

  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_exportFig_subsampleAggBias_folder(), "choro_fit_subsampleBias_seasIntensityRR_ss", regionText, "_S", s, ".png")
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

