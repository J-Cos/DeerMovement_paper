#######################
#fit models
#####################

library(terra)
library(tidyterra)
library(MuMIn)
library(tidyverse)
library(amt)

#load
bursts<-readRDS("Outputs/bursts.RDS")
sexmap<-readRDS("Outputs/sexmap.RDS") %>%
    droplevels()
r<-terra::rast("Outputs/CombinedRaster.tif")

# create random steps and add covariates
bursts<-bursts |> 
  mutate(steps = map(steps, function(x) {x |> random_steps(n_control = 15)} ))

bursts_withCovariates<-bursts |> 
  mutate(steps = map(steps, function(x) {x |> extract_covariates(r, where="end")} ))

df<-bursts_withCovariates |> select(id, steps) |> unnest(cols = steps) 

df<-df %>%
    left_join(., sexmap)

df<-df %>%
    mutate(landcover=as.numeric(as.character(landcover))) %>%
    mutate(landcover=case_when(
        landcover > 0 & landcover<200 ~ "Artificial",
        landcover > 200 & landcover<300 ~ "Agricultural",
        landcover > 300 & landcover<320 ~ "Forest",
        landcover > 320 & landcover<330 ~ "Scrub",
        landcover > 330 & landcover<340 ~ "Bare",
        )) %>%
    mutate(cos_ta = cos(ta_), 
        log_sl = log(sl_)) %>%
    mutate(season=lubridate::semester(t1_)) %>%
    mutate(season=as.factor(case_when(
        season ==2 ~ "Winter",
        season == 1 ~ "Summer"))) 

#explore some models
m0 <- df |> fit_clogit(case_ ~ landcover + elevation  + strata(id) + strata(step_id_))
summary(m0)

m1 <- df |> fit_clogit(case_ ~ landcover:sex + elevation:sex  + strata(id) + strata(step_id_))
summary(m1)

m2 <- df |> fit_clogit(case_ ~ landcover:cos_ta + landcover:log_sl + log_sl*cos_ta + strata(id) + strata(step_id_))
summary(m2)

m3 <- df |> fit_clogit(case_ ~ landcover + landcover:cos_ta + landcover:log_sl + log_sl*cos_ta + strata(id) + strata(step_id_))
summary(m3)

m3b <- df |> fit_clogit(case_ ~ landcover + landcover:cos_ta + landcover:log_sl + log_sl:cos_ta + strata(id) + strata(step_id_))
summary(m3b)

m3c <- df |> fit_clogit(case_ ~ landcover + landcover:cos_ta + landcover:log_sl + log_sl*cos_ta + elevation+ strata(id) + strata(step_id_))
summary(m3c)

m3d <- df |> fit_clogit(case_ ~ landcover + landcover:cos_ta + landcover:log_sl + log_sl*cos_ta + elevation+ landcover:elevation+ strata(id) + strata(step_id_))
summary(m3d)

m3e <- df |> fit_clogit(case_ ~ landcover:season + landcover:cos_ta + landcover:log_sl + log_sl*cos_ta + elevation:season+ landcover:elevation+ strata(id) + strata(step_id_))
summary(m3e)

m3f <- df |> fit_clogit(case_ ~ landcover*elevation*season*sex + strata(id) + strata(step_id_))
summary(m3f)

m3g <- df |> fit_clogit(case_ ~ landcover+ landcover:season + landcover:sex + landcover:season:sex + strata(id) + strata(step_id_))
summary(m3g)

m3h <- df |> fit_clogit(case_ ~ landcover+ landcover:season + landcover:sex + landcover:season:sex + cos_ta*log_sl + strata(id) + strata(step_id_))
summary(m3h)

m4 <- df |> fit_clogit(case_ ~ landcover:cos_ta + landcover:log_sl + log_sl + cos_ta + strata(id) + strata(step_id_))
summary(m4)

m5 <- df |> fit_clogit(case_ ~ landcover + landcover:log_sl + landcover:cos_ta +elevation +  elevation:log_sl + elevation:cos_ta + log_sl +cos_ta+ strata(id) + strata(step_id_))
summary(m5)

m6 <- df |> fit_clogit(case_ ~ landcover + landcover:log_sl + landcover:cos_ta +elevation + elevation:cos_ta + log_sl +cos_ta+ strata(id) + strata(step_id_))
summary(m6)

m7 <- df |> fit_clogit(case_ ~ landcover + landcover:log_sl + landcover:cos_ta + log_sl +cos_ta+ strata(id) + strata(step_id_))
summary(m7)

m8 <- df |> fit_clogit(case_ ~ landcover*season*sex*log_sl + elevation*season + distance*cos_ta*sex + strata(id) + strata(step_id_))
summary(m8)

m9 <- df |> fit_clogit(case_ ~ landcover*sex*log_sl + elevation*season + distance*cos_ta*sex + strata(id) + strata(step_id_))
summary(m9)

m10 <- df |> fit_clogit(case_ ~ landcover*season*sex*log_sl + elevation*season + distance*cos_ta*sex + log_sl*cos_ta + strata(id) + strata(step_id_))
summary(m10)

m11 <- df |> fit_clogit(case_ ~ landcover*season*sex + elevation*season + distance*sex + strata(id) + strata(step_id_))
summary(m11)

m12 <- df |> fit_clogit(case_ ~ landcover*sex*season*elevation*distance + strata(id) + strata(step_id_))
summary(m12)

m13 <- df |> fit_clogit(case_ ~ landcover*sex*season*elevation+distance:sex + strata(id) + strata(step_id_))
summary(m13)

AIC(m0)
AIC(m1)
AIC(m2)
AIC(m3)
AIC(m3b)
AIC(m3c)
AIC(m3d)
AIC(m3e)
AIC(m3f)
AIC(m3g)
AIC(m3h)
AIC(m4)
AIC(m5)
AIC(m6)
AIC(m7)
AIC(m8)
AIC(m9)
AIC(m10)
AIC(m11)
AIC(m12)
AIC(m13)
#m10 is best

#save an example model
sink("Outputs/ExampleModel.txt")
summary(m13)