# 12/22/16: separate files for calculate_residuals functions
# load this function when response variable is y: normal likelihood

################################

calculate_residuals <- function(fitDat){
  # calculations that depend on fitted values for non-zero values: raw residuals, std residuals, 95% CI for fitted values
  print(match.call())

  returnDat <- fitDat %>%
    mutate(yhat_resid = (y-mean)/sd) %>%
    mutate(yhat_rawresid = (y-mean)) %>%
    mutate(LB = q_025, UB = q_975)
  
  return(returnDat)
}