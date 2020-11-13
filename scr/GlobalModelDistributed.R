setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")

dff = load.data.xz1()

# dlagm for single time series only 
library(dLagM)
data(M1Germany)
data = M1Germany[1:144,]
model.ardlDlm  = ardlDlm(formula = logprice ~ interest + logm1,
                         data = data.frame(data) , p = 2 , q = 1 )

# library(dlnm) for single time series only 