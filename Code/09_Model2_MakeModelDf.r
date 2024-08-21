#######################
#MODEL 2 - Make new model df
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
dists<-terra::rast("Outputs/ReleaseDistances.tif")


# create random steps and add covariates - now includingcovariate extraction rfom a raster of distance to release for each deer
bursts<-bursts |> 
  mutate(steps = map(steps, function(x) {x |> random_steps(n_control = 15)} ))

bursts_withCovariates<-bursts |> 
  mutate(steps = map(steps, function(x) {x |> extract_covariates(r, where="end")} )) |>
  mutate(steps = map(steps, function(x) {x |> extract_covariates(dists, where="end")} ))

df<-bursts_withCovariates |> select(id, steps) |> unnest(cols = steps) 

df<-df %>%
    left_join(., sexmap)

df<-df %>%
    mutate(cos_ta = cos(ta_), 
        log_sl_100m = log(sl_/100),
        log_distance = log(distance)) %>%
    mutate(season=getTwoSeasons(t1_))


# simplify the distance to release columns down to a single column with the correct values for each deer
df_withDistance2Release<-df %>% pivot_longer(Roberto:Banditu, names_to="raster", values_to="distance2release") %>%
    filter(id==raster) %>%
    select(-raster) %>%
    mutate(log_distance2release=log(distance2release))
saveRDS(df_withDistance2Release, "Outputs/Model2Dataframe.RDS")