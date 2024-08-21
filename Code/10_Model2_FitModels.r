#######################
#Model 2 - fit models
#####################

library(MuMIn)
library(tidyverse)
library(amt)

#load
df<-readRDS("Outputs/Model2Dataframe.RDS")


########################
#modelling
###########################
# previous best model (model 6 in previous round)
m0 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + landcover:distance +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m0)
#same
summary(m0) 

#add log_ditance2release

m1 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta + log_distance2release+
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + landcover:distance +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))


AIC(m1)
summary(m1) 


#therfore m6 best
sink("Outputs/BestModel2.txt")
summary(m1)
closeAllConnections()


###########################################################################################################
# end of code
#######################################################################################################