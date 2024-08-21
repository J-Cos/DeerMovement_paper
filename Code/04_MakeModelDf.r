#######################
#fit models
#####################

library(terra)
library(tidyterra)
library(MuMIn)
library(tidyverse)
library(amt)

source(list.files("Code/Functions", full.names=TRUE))

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
    mutate(cos_ta = cos(ta_), 
        log_sl_100m = log(sl_/100),
        log_distance = log(distance)) %>%
    mutate(season=getTwoSeasons(t1_))

saveRDS(df, "Outputs/ModelDataframe.RDS")