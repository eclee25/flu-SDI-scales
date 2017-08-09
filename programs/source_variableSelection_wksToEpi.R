
## Name: Elizabeth Lee
## Date: 8/9/17
## Function: functions to perform EDA for variable selection generally -- functions for both cty and st levels
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(GGally) 
require(INLA)

#### functions for pairwise comparison ################################
pairs_scatterplotMatrix <- function(full_df){
  # return scatterplot matrix of all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>%
    mutate(logy = log(y)) %>%
    select(y, logy, logE, contains("_")) %>%
    filter(y > 0)
  
  pairPlt <- ggpairs(datOnly) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  return(pairPlt)
}
################################

pairs_corrMatrix <- function(full_df){
  # return correlation matrix for all variables pooled across states & seasons
  print(match.call())
  
  rvLabs <- label_tot_predictors()
  
  datOnly <- full_df %>%
    mutate(fipsSeas = paste(fips, season, sep="-")) %>%
    select(fipsSeas, contains("_")) %>%
    select(-fips_st, -graphIdx_st) %>%
    gather(RV, value, contains("_")) %>%
    clean_RVnames(.) %>%
    left_join(rvLabs, by = "RV") %>%
    select(-RV) %>%
    mutate(pltLabs = factor(pltLabs, levels = rvLabs$pltLabs)) %>%
    spread(pltLabs, value, drop = TRUE) %>%
    select(-fipsSeas)

  return(ggcorr(datOnly, method = c("pairwise", "spearman"), label = TRUE, high = "#3B9AB2", low = "#F21A00", legend.position = "bottom", layout.exp = 1, hjust = 0.85))
}


#### functions for single variable modeling ################################
subset_singleVariable_data <- function(full_df, s, covariate){
  # subset data for single response & covariate modeling
  print(match.call())
  
  # subset data according to season and covariate in function arguments
  mod_df <- full_df %>%
    filter(season == s & !is.na(lat) & !is.na(lon)) %>%
    rename_(varInterest = covariate) %>%
    mutate(fips_st = substr(fips, 1, 2)) %>%
    rename(regionID = region) %>%
    select(fips, fips_st, regionID, season, logy, logE, varInterest) %>%
    mutate(ID = seq_along(fips))
  
  return(mod_df)
}
################################

model_singleVariable_inla <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff
  # 8/9/17 rm offset
  print(match.call())
  
  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") +
    f(graphIdx_nonzero, model = "besag", graph = path_adjMxExport_cty) +
    f(fips_st_nonzero, model = "iid") +
    f(regionID_nonzero, model = "iid") +
    f(season_nonzero, model = "iid") +
    intercept_nonzero + varInterest 


  mod <- inla(formula, family = "poisson", data = mod_df, 
                control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = 1),
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE) 
  
  names(mod$summary.fixed) <- c("mean", "sd", "LB", "q_5", "UB", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mean, sd, LB, UB) 
  
  # data to save
  coefRow <- list(respCode = respCode, RV = covariate, exportDate = as.character(Sys.Date()), mean = modOutput$mean, sd = modOutput$sd, LB = modOutput$LB, UB = modOutput$UB, DIC = mod$dic$dic)
  
  return(coefRow)
}
################################

model_singleVariable_inla_iid <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff, excluding spatial CAR term
  print(match.call())
  
  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") +
    f(fips_st_nonzero, model = "iid") +
    f(regionID_nonzero, model = "iid") +
    f(season_nonzero, model = "iid") +
    intercept_nonzero + varInterest 


  mod <- inla(formula, family = "poisson", data = mod_df, 
                control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE) 
  
   names(mod$summary.fixed) <- c("mean", "sd", "LB", "q_5", "UB", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mean, sd, LB, UB) 
  
  # data to save
  coefRow <- list(respCode = respCode, RV = covariate, exportDate = as.character(Sys.Date()), mean = modOutput$mean, sd = modOutput$sd, LB = modOutput$LB, UB = modOutput$UB, DIC = mod$dic$dic)
  
  return(coefRow)
}
################################

model_singleVariable_inla_noSeas <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff, excluding season random effects
  print(match.call())
  
  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") +
    f(graphIdx_nonzero, model = "besag", graph = path_adjMxExport_cty) +
    f(fips_st_nonzero, model = "iid") +
    f(regionID_nonzero, model = "iid") +
    intercept_nonzero + varInterest 


  mod <- inla(formula, family = "poisson", data = mod_df, 
                control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE) 
  
  names(mod$summary.fixed) <- c("mean", "sd", "LB", "q_5", "UB", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mean, sd, LB, UB)  
  
  # data to save
  coefRow <- list(respCode = respCode, RV = covariate, exportDate = as.character(Sys.Date()), mean = modOutput$mean, sd = modOutput$sd, LB = modOutput$LB, UB = modOutput$UB, DIC = mod$dic$dic)
  
  return(coefRow)
}
################################

model_singleVariable_inla_noSt <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff, rm state random effects
  print(match.call())
  
  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") +
    f(graphIdx_nonzero, model = "besag", graph = path_adjMxExport_cty) +
    f(regionID_nonzero, model = "iid") +
    f(season_nonzero, model = "iid") +
    intercept_nonzero + varInterest
  
  
  mod <- inla(formula, family = "poisson", data = mod_df, 
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE) 
  
   names(mod$summary.fixed) <- c("mean", "sd", "LB", "q_5", "UB", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mean, sd, LB, UB) 
  
  # data to save
  coefRow <- list(respCode = respCode, RV = covariate, exportDate = as.character(Sys.Date()), mean = modOutput$mean, sd = modOutput$sd, LB = modOutput$LB, UB = modOutput$UB, DIC = mod$dic$dic)
  
  return(coefRow)
}
################################

model_singleVariable_inla_noRdm <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff, rm random effects except error term
  print(match.call())
  
  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    intercept_nonzero + varInterest 
  
  
  mod <- inla(formula, family = "poisson", data = mod_df, 
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE) 
  
   names(mod$summary.fixed) <- c("mean", "sd", "LB", "q_5", "UB", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mean, sd, LB, UB) 
  
  # data to save
  coefRow <- list(respCode = respCode, RV = covariate, exportDate = as.character(Sys.Date()), mean = modOutput$mean, sd = modOutput$sd, LB = modOutput$LB, UB = modOutput$UB, DIC = mod$dic$dic)
  
  return(coefRow)
}
################################

#### functions for single variable coef plotting ################################
plot_singleVarCoef <- function(coefDat){
  # plot all coef modes, Q.025 - Q0.975 over time
  print(match.call())
  
  figure <- ggplot(coefDat, aes(x = RV, y = mean, colour = signif)) +
    geom_pointrange(aes(ymin = LB, ymax = UB)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("red", "grey75")) +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12), text = element_text(size = 12), legend.margin = margin())
  
  return(figure)
}

################################
label_tot_predictors <- function(){
    cleanRV <- c("humidity", "anomHumidity", "temperature", "latitude", "sourceLocDist", "pollution", "socialOrgs", "popdensity", "housdensity", "commute", "flight", "infantToddler", "child", "adult", "elderly", "vaxcovI", "vaxcovE", "priorImmunity", "fluPos", "H3", "H3A", "B", "hospaccess", "physaccess", "medcost", "avgHHSize", "singlePersonHH", "singleParentHH", "poverty", "income", "mcaid", "mcare", "poorHealth", "unhealthyDays", "insured", "imscoverage", "careseekT", "careseekC", "careseekA")
    pltLabels <- c("humidity", "anomHumidity", "temp", "lat", "srcDist", "pollution", "socialOrgs", "popDensity", "householdSize", "commute", "flight", "infant", "child", "adult", "elderly", "toddlerVacc", "elderlyVacc", "priorImmunity", "fluPos", "fluH3", "fluH3A", "fluB", "hospAccess", "physAccess", "medcost", "avgHHSize", "1PersonHH", "1ParentHH", "poverty", "income", "mcaid", "mcare", "poorHealth", "unhealthyDays", "insured", "claimsCoverage", "careseek", "careseekC", "careseekA")

    dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
    return(dfLabels)
}