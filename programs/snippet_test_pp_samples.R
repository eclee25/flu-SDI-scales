# test inla posteior samples code
# 7/3/17

require(INLA)
require(tidyverse)
require(microbenchmark)

df <- tbl_df(data.frame(y=rnorm(10), E=rnorm(10))) %>%
  mutate(logE = log(abs(E))) %>%
  mutate(rr = y - logE)
print(microbenchmark(rr_lincomb <- inla.make.lincombs(Predictor = 1, offset = -1)))
print(microbenchmark(modelOutput <- inla(y ~ 1+offset(logE), data = df,
                                         offset = logE,
                                         # lincomb = rr_lincomb,
                                         control.predictor = list(compute = TRUE),
                                         control.compute = list(config = TRUE),
                                         verbose = TRUE)))
fitMarginals_rr <- list()
summ_rr_df <- data.frame()
bayesPvalue_rr <- rep(NA, nrow(df))
hpdInterval_rr_df <- data.frame()
numrep <- 20
replicates_rr_df <- matrix(NA, nrow = nrow(df), ncol = numrep)

# draw posterior samples from the latent field for x, last item is the intercept that needs to be added to the linear predictor
print(microbenchmark(postSamples <- inla.posterior.sample(numrep, modelOutput)))
# transform those posterior samples into a matrix
print(microbenchmark(postSamplesMx <- t(sapply(postSamples, function(x) x$latent))))

for(i in 1:nrow(df)){
  print(i)
  
  # transform marginal to relative risk
  print(microbenchmark(fitMarginals_rr[[i]] <- inla.tmarginal(function(x) (x - df$E[i]), modelOutput$marginals.fitted.values[[i]])))
  
  # summary statistics for rr marginal
  print(microbenchmark(summaryRow <- inla.zmarginal(fitMarginals_rr[[i]], silent = TRUE)))
  summ_rr_df[i,] <- summaryRow
  
  # draw replicates from rr marginal
  print(microbenchmark(replicatesRow <- inla.rmarginal(numrep, fitMarginals_rr[[i]])))
  replicates_rr_df[i,] <- replicatesRow
  
  # calculate Bayesian p-value for rr marginal
  print(microbenchmark(bayesPvalue_rr[i] <- inla.pmarginal(q = df$rr[i], marginal = fitMarginals_rr[[i]])))
  
  # calculate high probability density (HPD) interval for relative risk fits
  print(microbenchmark(hpdRow <- inla.hpdmarginal(0.95, fitMarginals_rr[[i]])))
  hpdInterval_rr_df[i,] <- hpdRow
  
}