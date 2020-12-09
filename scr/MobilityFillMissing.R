# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1CLYWS


############################### read ###############################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data()
 
############################### apply the selection 62 counties ###############################
kp.row = mb.missing.fips$FIPS[(mb.missing.fips$work<=30)&(mb.missing.fips$retail<=30)]
dff$kp = as.factor(as.character((dff$FIPS %in% kp.row)*1))

# c(41003, 41005, 41017, 41019, 41029, 41033, 41039, 41043, 41047, 41051, 
#   41067, 41071, 53005, 53011, 53015, 53033, 53035, 53041, 53053, 53057, 
#   53061, 53063, 53067, 53073, 53077, 6001, 6007, 6013, 6017, 6019, 6023, 
#   6025, 6029, 6031, 6037, 6039, 6041, 6047, 6053, 6055, 6057, 6059, 6061, 
#   6065, 6067, 6071, 6073, 6075, 6077, 6079, 6081, 6083, 6085, 6087, 6089, 
#   6095, 6097, 6099, 6101, 6107, 6111, 6113)

