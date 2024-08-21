#######################
#MODEL 2 - get release distance rasters
#####################

library(terra)
library(tidyterra)
library(tidyverse)

#load
r<-terra::rast("Outputs/CombinedRaster.tif")
releasePoints<-vect("Outputs/releasePoints") %>%
    project(., crs(r))

distance<-list()
for (i in 1:length(releasePoints)) {
    distance[[i]]<-distance(x=r[[4]], y=releasePoints[i])
    names(distance[[i]])<-releasePoints$id[i]
}
dists<-rast(distance)

terra::writeRaster(dists, "Outputs/ReleaseDistances.tif", overwrite=TRUE)
