# 12/22/16: separate files for calculate_residuals functions

################################

calculate_residuals <- function(fitDat){
  # calculations that depend on fitted values for non-zero values: raw residuals, std residuals, 95% CI for fitted values
  print(match.call())

  returnDat <- fitDat %>%
    mutate(yhat_resid = (y1-mean)/sd) %>%
    mutate(yhat_rawresid = (y1-mean)) %>%
    rename(LB = q_025, UB = q_975)
  
  return(returnDat)
}