
## Name: Elizabeth Lee
## Date: 2/8/16
## Function: functions to export INLA results as data files and diagnostic figures -- state scale 
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(scales); require(classInt); require(data.table)

#### functions for diagnostic plots  ################################

plot_stateChoro <- function(exportPath, path_csv_abbr, inDat, pltVarTxt, code, zeroes){
# draw state choropleth with tiers or gradient colors and export to file
  print(match.call())

  # plot formatting
  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300

  # append state name to plot data
  abbrDat <- read_csv(path_csv_abbr, skip = 1, col_names = c("state", "abbr_st", "fips_st"), col_types = "ccc") %>%
    mutate(state = tolower(state))
  pltDat <- left_join(inDat, abbrDat, by = "fips_st")

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

    choro <- ggplot(pltDat2, aes(map_id = state)) +
      geom_map(map = states_map, aes(fill = pltVarBin), color = "grey25", size = 0.15) +
      scale_fill_brewer(name = pltVarTxt, palette = "RdYlGn", label = breakLabels, na.value = "grey60") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")       
  }

  # gradient choropleth
  else if (code == 'gradient'){
    # data for gradient has minimal processing
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt) 

    choro <- ggplot(pltDat, aes(map_id = state)) +
      geom_map(map = states_map, aes(fill = pltVar), color = "grey25", size = 0.15) +
      scale_fill_continuous(name = pltVarTxt, low = "#f0fff0", high = "#006400") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)  
}
################################

plot_state_choropleth <- function(exportPath, pltDat, pltVarTxt, code){
# draw state choropleth with tiers or gradient colors and export to file
  print(match.call())

  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300
  pltDat <- pltDat %>% rename_(pltVar = pltVarTxt)

  if (code == 'tier'){
    choro <- ggplot(pltDat, aes(map_id = state)) +
      geom_map(aes(fill = pltVar), map = states_map, color = "black") +
      scale_fill_brewer(palette = "RdYlGn") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  }

  else if (code == 'gradient'){
    choro <- ggplot(pltDat, aes(map_id = state)) +
      geom_map(aes(fill = pltVar), map = states_map, color = "black") +
      scale_fill_continuous(low = "green", high = "red") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)  
  
}
################################

export_summaryStats_fitted_hurdle_st <- function(exportPath, oneLik_fits, modDataFullOutput, modCodeString, dbCodeString, season){
  # process binomial likelihood or gamma likelihood fitted values for diagnostic plotting
  print(match.call())
  
  names(oneLik_fits) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode")
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips_st, ID, y, y1, season), oneLik_fits) %>% 
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, exportDate = as.character(Sys.Date())) %>% # 10/11/16: grab season from modDataFullOutput instead of function argument
    select(modCodeStr, dbCodeStr, season, exportDate, fips_st, ID, mean, sd, q_025, q_5, q_975, mode, y, y1)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}


