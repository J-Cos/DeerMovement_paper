#######################
#fit models
#####################

library(MuMIn)
library(tidyverse)
library(amt)

#load
df<-readRDS("Outputs/ModelDataframe.RDS")


########################
#modelling
###########################
# saturated plausible model
spm <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + log_sl_100m:sex + landcover:distance + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season + 
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
summary(spm)
AIC(spm)

######################
#complex down
#######################

#remove log_sl_100m:sex:season:landcover
m2 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + log_sl_100m:sex + landcover:distance + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season + 
    strata(id) + strata(step_id_))
AIC(m2)
#worse

# remove log_sl_100m:sex:season 
m3 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + log_sl_100m:sex + landcover:distance + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m3)
#same
summary(m3)

# remove distance:season
m4 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + log_sl_100m:sex + landcover:distance + season:slope +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m4)
#worse
summary(m4)

#remove season:slope
m5 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + log_sl_100m:sex + landcover:distance +distance:season +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m5)
#worse
summary(m5)

#remove  log_sl_100m:sex 
m6 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + landcover:distance + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m6)
#same
summary(m6) 

# remove landcover:distance 
m7 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + cos_ta:landcover + log_sl_100m:slope + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m7)
#worse

# remove cos_ta:landcover 
m8 <- df |> fit_clogit(
    case_ ~ 
    landcover + slope + distance + log_sl_100m + cos_ta +
    log_sl_100m:landcover + cos_ta:distance + log_sl_100m:slope + landcover:distance + season:slope +distance:season +cos_ta:slope +
    log_sl_100m:sex:season:landcover +
    strata(id) + strata(step_id_))
AIC(m8)
#worse

#therfore m6 best
sink("Outputs/BestModel.txt")
summary(m6)
closeAllConnections()


###########################################################################################################
# end of code
#######################################################################################################