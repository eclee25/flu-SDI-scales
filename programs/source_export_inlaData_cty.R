
## Name: Elizabeth Lee
## Date: 6/6/16
## Function: functions to export INLA results as data files and diagnostic figures -- specific to county scale 
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(maps); require(scales); require(classInt); require(data.table)

#### functions for diagnostic plots  ################################

plot_countyChoro <- function(exportPath, pltDat, pltVarTxt, code, zeroes){
# draw state choropleth with tiers or gradient colors and export to file
  print(match.call())
  
  countyMap <- map_data("county")
  data(county.fips)
  # plot formatting
  h <- 5; w <- 8; dp <- 300

  # merge county data
  polynameSplit <- tstrsplit(county.fips$polyname, ",")
  ctyMap <- tbl_df(county.fips) %>%
    mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
    mutate(region = polynameSplit[[1]]) %>%
    mutate(subregion = polynameSplit[[2]]) %>%
    full_join(countyMap, by = c("region", "subregion")) %>%
    filter(!is.na(polyname) & !is.na(long)) %>%
    rename(state = region, county = subregion) %>%
    rename(region = fips) %>%
    select(-polyname)
  
  # tier choropleth
  if (code == 'tier'){
    # process data for tiers
    # 7/21/16: natural breaks w/ classIntervals
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt) 
    # create natural break intervals with jenks algorithm
    intervals <- classIntervals(pltDat$pltVar[!is.na(pltDat$pltVar)], n = 5, style = "jenks")
    if (zeroes){
      # 0s have their own color
      if (0 %in% intervals$brks){
        breakList <- intervals$brks
      } else {
        breakList <- c(0, intervals$brks)
      }
      breaks <- sort(c(0, breakList))
    } else{
      breaks <- c(intervals$brks)
    }
    breaksRound <- round(breaks, 1) 
    breakLabels <- matrix(1:(length(breaksRound)-1))
    for (i in 1:length(breakLabels)){
      # create legend labels
      breakLabels[i] <- paste0("(",as.character(breaksRound[i]), "-", as.character(breaksRound[i+1]), "]")}
    # reverse order of break labels so zeros are green and larger values are red
    breakLabels <- rev(breakLabels) 
    pltDat2 <- pltDat %>%
      mutate(pltVarBin = factor(.bincode(pltVar, breaks, right = TRUE, include.lowest = TRUE))) %>%
      mutate(pltVarBin = factor(pltVarBin, levels = rev(levels(pltVarBin))))

    
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat2, map = ctyMap, aes(fill = pltVarBin, map_id = fips), color = "grey25", size = 0.15) +
      scale_fill_brewer(name = pltVarTxt, palette = "RdYlGn", label = breakLabels, na.value = "grey60") +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  }

  # gradient choropleth
  else if (code == 'gradient'){
    # data for gradient has minimal processing
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt) 

    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id=region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = pltVar, map_id = fips), color = "grey25", size = 0.15) +
      scale_fill_continuous(name = pltVarTxt, low = "#f0fff0", high = "#006400") +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  }

  ggsave(exportPath, choro, height = h, width = w, dpi = dp)  
  
}

################################

