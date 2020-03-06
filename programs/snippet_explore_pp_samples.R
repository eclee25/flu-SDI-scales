# explore posterior samples for dummy 11a V1
require(tidyverse)

samp <- read_csv("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/scales/R_export/inlaModelData_export/11a_iliSum_v1-4/posteriorSamples_normal_11a_iliSum_v1-4.csv")
hist(samp$Predictor.001)
hist(samp$Predictor.002)
hist(samp$Predictor.014)
