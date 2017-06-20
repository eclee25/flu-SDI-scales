
## Name: Elizabeth Lee
## Date: 3/17/17
## Function: functions to perform posterior predictive checks from INLA data
## Filenames: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(readr)
setwd(dirname(sys.frame(1)$ofile))

#### functions for model data import and cleaning ################################

################################
string_coef_fname <- function(modCodeStr){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/summaryStats_", modCodeStr, ".csv"))
}
################################
import_coef_dat <- function(modCodeStr){
	coefDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cc_dd_____")
	return(coefDat)
}
################################


#### functions for model data import and cleaning ################################

export_cpoPIT_observations <- function(exportPath, modelOutput){
    # export CPO and PIT for individual observations
    print(match.call())

    cpoLs <- modelOutput$cpo$cpo
    failLs <- modelOutput$cpo$failure
    pitLs <- modelOutput$cpo$pit
    exportDat <- data.frame(ID = seq_along(cpoLs), cpo = cpoLs, cpoFail = failLs, pit = pitLs)
    write_csv(exportDat, exportPath)
}

################################
export_predictiveChecks_model11a <- function(path_csvExport_replicateData, path_csvExport_repSummData, modelOutput, modelData, modCodeStr){
    # export replicates and summary statistics for predictive checks
    print(match.call())

    # !set number of replicates to draw!
    numrep <- 1

    modelData2 <- modelData %>%
    select(ID, logy1_st_nonzero, y1) %>%
    mutate(modCodeStr = modCodeStr) %>%
    mutate(obs_rr = y1 - logy1_st_nonzero) %>%
    select(modCodeStr, ID, logy1_st_nonzero, y1, obs_rr)

    fitMarginals_rr <- c()
    summ_rr_df <- as.data.frame(matrix(NA, ncol = 7, nrow = nrow(modelData)))
    replicates_rr_df <- as.data.frame(matrix(NA, ncol = numrep, nrow = nrow(modelData)))
    bayesPvalue_rr <- c()
    hpdInterval_rr_df <- as.data.frame(matrix(NA, ncol = 2, nrow = nrow(modelData)))

    for(i in 1:nrow(modelData)){
        # transform marginal to relative risk
        fitMarginals_rr[[i]] <- inla.tmarginal(function(x) (x - modelData2$logy1_st_nonzero[i]), modelOutput$marginals.fitted.values[[i]])

        # summary statistics for rr marginal
        summaryRow <- inla.zmarginal(fitMarginals_rr[[i]], silent = TRUE)
        summ_rr_df[] <- summaryRow

        # draw replicates from rr marginal
        replicatesRow <- inla.rmarginal(numrep, fitMarginals_rr[[i]])
        replicates_rr_df[i,] <- replicatesRow

        # calculate Bayesian p-value for rr marginal
        bayesPvalue_rr[i] <- inla.pmarginal(q = modelData2$obs_rr[i], marginal = fitMarginals_rr[[i]])

        # calculate high probability density (HPD) interval for relative risk fits
        hpdRow <- inla.hpdmarginal(0.95, fitMarginals_rr[[i]])
        hpdInterval_rr_df[i,] <- hpdRow

    }

    names(replicates_rr_df) <- paste0("rep", 1:ncol(replicates_rr_df))
    names(summ_rr_df) <- c("mean", "sd", "q_025", "q_25", "q_5", "q_75", "q_975")
    names(hpdInterval_rr_df) <- c("hpd95_LB", "hpd95_UB")
    
    # export replicate samples
    exportReplicateData <- modelData2 %>%
    select(-logy1_st_nonzero, -y1) %>%
    bind_cols(replicates_rr_df)
    write_csv(exportReplicateData, path_csvExport_replicateData)
    
    # export replicate summary information
    exportRepSummData <- bind_cols(modelData2, summ_rr_df, hpdInterval_rr_df) %>% 
    select(-logy1_st_nonzero, -y1) %>%
    mutate(bayesPvalue_rr = bayesPvalue_rr) 
    write_csv(exportRepSummData, path_csvExport_repSummData)
    
}
################################

#### functions for model diagnostics ################################
string_ppReplicates_fname <- function(modCodeStr){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/ppReplicates_", modCodeStr, ".csv"))
}
################################
string_ppSummary_fname <- function(modCodeStr){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/ppSummary_", modCodeStr, ".csv"))
}
################################
import_ppReplicates <- function(modCodeStr){
    print(match.call())

    # import ids
    idsDat <- read_csv(string_ids_fname(modCodeStr))
    
    # import and clean replicate data
    returnDat <- read_csv(string_ppReplicates_fname(modCodeStr)) %>%
    gather(datatype, value, obs_rr:rep1) %>%
    left_join(idsDat, by = "ID") %>%
    mutate(rep = ifelse(substring(datatype, 1, 3)=="rep", TRUE, FALSE)) %>%
    mutate(repnum = ifelse(rep, as.numeric(substring(datatype, 4, nchar(datatype))), NA))

    return(returnDat)

}