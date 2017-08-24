
## Name: Elizabeth Lee
## Date: 8/16/17
## Function: functions to perform quantitative comparisons of models
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(tidyverse); require(data.table); require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))

################################
choro_pairFitCompare_overlap <- function(modCodeLs, pltFormats, datFormats){
  print(match.call())
  # examine overlap in fitted value distributions
  
  stopifnot(length(modCodeLs) == 2L)
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasLabels <- label_seas_predictors()
  exportFname <- paste0(string_exportFig_folder(), "choro_fitCompare_", pltFormats$descrip, ".png")
  modLabelsDf <- tbl_df(data.frame(modCodeStr = pltFormats$lvls, modLabs = pltFormats$labs, stringsAsFactors = FALSE))

  # data formatting
  fit_dataType <- datFormats$fit_dataType
  refModCode <- datFormats$refModCode
  
  # grab fitData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
  
  for (modCode in modCodeLs){
    if (fit_dataType == "posteriorSamples"){
      importDat <- import_fit_posteriorSamples(modCode)

    } else if (fit_dataType == "summaryStats"){
      importDat <- import_fit_summaryStats(modCode)
    }
    
    # if importing state level data, augment to county level
    if(is.null(importDat$fips)){ # causes a warning about unknown/uninitialized column 'fips'
      refDat <- import_fit_summaryStats(refModCode) %>%
        mutate(fips_st = substring(fips, 1, 2)) %>%
        select(season, fips, fips_st)
      importDat <- full_join(refDat, importDat, by = c("season", "fips_st"))
    }

    modDat <- importDat %>%
      mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>%
      select(modCodeStr, season, fips, LB, UB) 
    fullDf <- bind_rows(fullDf, modDat)
    print(paste("choro_pairFitCompare_overlap imported", modCode))
  }

  # prepare data for plotting
  args <- name_intervals(pltFormats$labs)
  pltDat <- fullDf %>%
    gather(bound, value, LB:UB) %>%
    left_join(modLabelsDf, by = "modCodeStr") %>%
    mutate(bound_spread = paste(modLabs, bound, sep = "_")) %>%
    select(-bound, -modLabs, -modCodeStr) %>%
    spread(bound_spread, value) %>%
    overlapping_intervals(args$intervalA_LB, args$intervalA_UB, args$intervalB_LB, args$intervalB_UB) %>%
    mutate(season = factor(paste0("S", season), levels = seasLabels$RV, labels = seasLabels$pltLabs))

  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = pltDat, map = ctyMap, aes(fill = overlap, map_id = fips), color = "grey50", size = 0.05) +
    scale_fill_manual(name = "", values = c("1" = "grey75", "0" = "#7b3294"), breaks = c("1", "0"), labels = c("match", "no match"), na.value = "grey75") +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = c(.9,.3)) +
    facet_wrap(~season, nrow=2)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
}
################################
choro_obsFitCompare_overlap <- function(modCodeLs, pltFormats, datFormats){
  print(match.call())
  # examine whether observed value falls within IQR of fitted value distribution (for a single model, but code can be run for all models in a modCodeLs)
  
  for (i in 1:length(modCodeLs)){
    modCode <- modCodeLs[i]
    abbrModCode <- pltFormats$descrip[i] 

    ## plot formatting ##
    w <- pltFormats$w; h <- pltFormats$h; dp <- 300
    seasLabels <- label_seas_predictors()
    exportFname <- paste0(string_exportFig_folder(), "choro_obsFitCompare_", abbrModCode, ".png")

    ## data formatting ##
    fit_dataType <- datFormats$fit_dataType
    refModCode <- datFormats$refModCode

    ## import fitted value distributions ##
    # grab fitData from multiple models
    fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
    
    # import fitted value distributions from posterior samples or summary statistics
    if (fit_dataType == "posteriorSamples"){
      postDat <- import_fit_posteriorSamples(modCode)
      obsDat <- import_fit_summaryStats(modCode) %>%
        select(season, contains("fips"), y1)
      importDat <- full_join(postDat, obsDat) # if 'by' is not specified, the dfs should join by all common names across the two tables (season, contains("fips"))

    } else if (fit_dataType == "summaryStats"){
      importDat <- import_fit_summaryStats(modCode)
    }
    
    # if importing state level data, augment to county level
    if(is.null(importDat$fips)){ # causes a warning about unknown/uninitialized column 'fips'
      refDat <- import_fit_summaryStats(refModCode) %>%
        mutate(fips_st = substring(fips, 1, 2)) %>%
        select(season, fips, fips_st)
      importDat <- full_join(refDat, importDat, by = c("season", "fips_st"))
    }

    modDat <- importDat %>%
      mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>%
      select(modCodeStr, season, fips, LB, UB, y1) %>%
      rename(Observed = y1)
    fullDf <- bind_rows(fullDf, modDat)
    print(paste("choro_obsFitCompare_overlap imported", modCode))

    ## prepare data for plotting ##
    pltDat <- fullDf %>%
      overlapping_point_and_intervals("LB", "UB", "Observed") %>%
      mutate(season = factor(paste0("S", season), levels = seasLabels$RV, labels = seasLabels$pltLabs))

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    ## plot ##
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = overlap, map_id = fips), color = "grey50", size = 0.05) +
      scale_fill_manual(name = "", values = c("1" = "grey75", "0" = "#7b3294"), breaks = c("1", "0"), labels = c("match", "no match"), na.value = "grey75") +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = c(.9,.3)) +
      facet_wrap(~season, nrow=2)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  } # end modCodeLs loop
}
################################
label_seas_predictors <- function(){
  cleanRV <- paste0("S", 3:9)
  pltLabels <- c("2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")
  
  dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
  return(dfLabels)
}
################################
string_exportFig_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/inlaModelComparisons/"))
}
################################
string_fit_fname <- function(modCodeStr){
  searchDir <-  paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr)
  return(grep("summaryStatsFitted_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}
################################
string_fitSamples_fname <- function(modCodeStr){
  searchDir <-  paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr)
  return(grep("posteriorSamples_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}
################################
name_intervals <- function(modLabs){
  intervalA_LB <- paste0(modLabs[1], "_LB"); intervalA_UB <- paste0(modLabs[1], "_UB")
  intervalB_LB <- paste0(modLabs[2], "_LB"); intervalB_UB <- paste0(modLabs[2], "_UB")
  return(list(intervalA_LB=intervalA_LB, intervalA_UB=intervalA_UB, intervalB_LB=intervalB_LB, intervalB_UB=intervalB_UB))
}
################################
overlapping_intervals <- function(df, intervalA_LB, intervalA_UB, intervalB_LB, intervalB_UB){
  # logical, do intervals overlap?

  df %>%
    mutate_(overlap = interp(~ifelse((aLB <= bLB & bLB <= aUB) | (aLB <= bUB & bUB <= aUB) | (bLB <= aLB & aLB <= bUB) | (bLB <= aUB & aUB <= bUB), "1", "0"), aLB = as.name(intervalA_LB), aUB = as.name(intervalA_UB), bLB = as.name(intervalB_LB), bUB = as.name(intervalB_UB)))
                      
}
################################
overlapping_point_and_intervals <- function(df, interval_LB, interval_UB, point){
  # logical, does point fall between intervals?

  df %>%
    mutate_(overlap = interp(~ifelse((pt > LB) & (pt < UB), "1", "0"), pt = as.name(point), LB = as.name(interval_LB), UB = as.name(interval_UB)))
                      
}
################################
import_county_geomMap <- function(){
  print(match.call())
  
  countyMap <- map_data("county")
  data(county.fips)
  polynameSplit <- tstrsplit(county.fips$polyname, ",")
  county_geomMap <- tbl_df(county.fips) %>%
    mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
    mutate(region = polynameSplit[[1]]) %>%
    mutate(subregion = polynameSplit[[2]]) %>%
    full_join(countyMap, by = c("region", "subregion")) %>%
    filter(!is.na(polyname) & !is.na(long)) %>%
    rename(state = region, county = subregion) %>%
    rename(region = fips) %>%
    select(-polyname)
  
  return(county_geomMap)
}
################################
import_fit_posteriorSamples <- function(modCode){
  print(match.call())

  allSamplesDat <- read_csv(string_fitSamples_fname(modCode))
  fitSamplesDat <- allSamplesDat %>%
    select(contains("Predictor"))

  # import ids from summaryStats
  ids <- import_fit_summaryStats(modCode) %>%
    select(season, contains("fips"))

  gatherDat <- fitSamplesDat %>%
    gather(predictor, sample, contains("Predictor")) %>%
    group_by(predictor) %>%
    summarise(mean = mean(sample), sd = sd(sample), q_025 = quantile(sample, .025), q_5 = median(sample), q_975 = quantile(sample, .975))

  stopifnot(nrow(ids) == nrow(gatherDat)) # check for bind_cols
  summaryDat <- bind_cols(ids, gatherDat) %>%
    mutate(modCodeStr = modCode) %>% 
    select(modCodeStr, predictor, season, contains("fips"), mean, sd, q_025, q_5, q_975)

  return(summaryDat)
}

################################
import_fit_summaryStats <- function(modCode){
  print(match.call())

  summaryDat <- read_csv(string_fit_fname(modCode), col_types = "c_d_c_ddddd__d") # modCodeStr, season, fips/fips_st, mean, sd, q_025, q_5, q_975, y1 (response)

  return(summaryDat)
}