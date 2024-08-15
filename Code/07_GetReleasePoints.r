#######################
#get release points
#####################

library(terra)
library(tidyterra)
library(MuMIn)
library(tidyverse)
library(amt)

#load
df<-readRDS("Outputs/ModelDataframe.RDS")
r<-terra::rast("Outputs/CombinedRaster.tif")

# main code
releasePoints_df<-df %>% 
    filter(case_) %>%
    select(id, x1_, y1_, t1_) %>% 
    ungroup() %>%
    as.data.frame %>%
    group_by(id) %>%
    arrange(t1_) %>%
    filter(row_number()==1) 

releasePoints<-releasePoints_df %>%
    rename(lat=y1_, lon=x1_, time=t1_) %>% 
    tidyterra::as_spatvector()

crs(releasePoints)<-"EPSG:31467"
releasePoints<-project(releasePoints, "EPSG:4258")

r<-project(r, "EPSG:4258")
localArea<-crop(r, ext(buffer(releasePoints, width=10000)))

# make map
releaseMap<- ggplot() +
    geom_spatraster(data=localArea, aes(fill=landcover))+
    geom_spatvector(data= releasePoints, aes(color=time))
ggsave("Figures/releaseMap.pdf", releaseMap)

#export csv
writeVector(releasePoints, "Outputs/releasePoints")