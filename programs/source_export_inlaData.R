
## Name: Elizabeth Lee
## Date: 6/6/16
## Function: general functions to export INLA results as data files -- should apply to cty and st scales, but not implemented for state scale models
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2)

#### functions for results plots by model  ################################

################################
plot_rdmFx_marginalsSample<- function(path_plotExport_rdmFxSample, marginalsRandomID, rdmFx_RV){
  # plot a sample of marginal posteriors for random effects
  print(match.call())
  
  w <- 6; h <- 6; dp <- 200
  
  png(path_plotExport_rdmFxSample, width = w, height = h, units = "in", res = dp)
  par(mfrow = c(3, 2))
  for (i in 1:6){
    plot(marginalsRandomID[[i]], xlab = sprintf("%s%s", rdmFx_RV, i))
  }
  dev.off()
}
################################

plot_fixedFx_marginals <- function(exportPath, marginalsFixed, modCodeStr, s){
  # plot marginal posteriors for all fixed effect coefficients (pass entire INLA model output to function)
  print(match.call())
  
  w <- 4; h <- 4; dp <- 300
  
  names_fixedFx <- names(marginalsFixed)  # standard naming system "O_samplingeffort" or "X_driver"
  
  for (i in 1:length(names_fixedFx)){
    exportPath_full <- paste0(exportPath, sprintf("/inla_%s_%s_marg_S%s.png", modCodeStr, names_fixedFx[i], s))
    png(exportPath_full, width = w, height = h, units = "in", res = dp)
    par(mfrow = c(1, 1))
    plot(marginalsFixed[[i]], xlab = paste0(names_fixedFx[i], sprintf(", S%s", s)), 
         xlim = c(-4, 4), ylab = "density")
    dev.off()
  }
  
}
################################



#### functions for results plots by modcode  ################################

################################

plot_coefDistr_RV <- function(plotDat, path_plotExport_coefDistr, plotFilename){
  # plot all coef mean & 95%CI over RV
  print(match.call())
  
  # plot formatting
  w <- 8; h <- 4; dp <- 250

  # plot fixed effects
  plotOutput <- ggplot(plotDat, aes(x = RV, y = mean)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous("coefMean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("red", "#000080")) +
    guides(colour = FALSE) +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
  ggsave(paste0(path_plotExport_coefDistr, plotFilename), plotOutput, height = h, width = w, dpi = dp)
  
}
################################


#### functions for data export  ################################
################################

export_DIC <- function(exportPath, dicDataframe){
  # 6/7/16: export DIC & CPO values for one season (instead of all seasons)
  # if cpoFail > 0, CPO calculation is invalid
  print(match.call())
  
  # parse modCodeStr
  parsed <- strsplit(modCodeStr, "_")[[1]]
  
  # clean data frame
  dicOutput <- tbl_df(dicDataframe) %>%
    mutate(modCode = parsed[1], dbMetric = parsed[2], version = parsed[3]) %>%
    select(modCodeStr, modCode, dbMetric, version, season, exportDate, DIC, CPO, cpoFail)
  
  write_csv(dicOutput, exportPath)
  
}
################################

export_ids <- function(exportPath, modDataFullOutput){
  # export random and state/region group effect ids with true identities, as a key
  print(match.call())
  
  # 10/30/16 control flow for spatial structure
  if(is.null(modDataFullOutput$graphIdx) & is.null(modDataFullOutput$graphIdx_st)){ # without spatially structured terms
    ids <- modDataFullOutput %>% 
      select(season, fips, ID, st, regionID)
  } else if(is.null(modDataFullOutput$graphIdx_st) & !is.null(modDataFullOutput$graphIdx)){ # with spatially structured county term
    ids <- modDataFullOutput %>% 
      select(season, fips, ID, st, regionID, graphIdx)
  } else if(!is.null(modDataFullOutput$graphIdx_st) & is.null(modDataFullOutput$graphIdx)){ # with spatially structured state term
    ids <- modDataFullOutput %>% 
      select(season, fips, ID, st, regionID, graphIdx_st)
  } else{
    ids <- modDataFullOutput %>% 
      select(season, fips, ID, st, regionID, graphIdx, graphIdx_st)
  }
  
  # export data to file
  write_csv(ids, exportPath)
  
}
################################


