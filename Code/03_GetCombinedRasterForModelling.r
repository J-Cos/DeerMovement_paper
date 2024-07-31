##################################
# Get combined raster in same projection as gps data
#####################################

library(terra)
library(tidyterra)
library(tidyverse)

# load rasters from GEE
e<-rast("Data/Corsica_elevation.tif")
s<-rast("Data/Corsica_slope.tif")
lc<-as.factor(rast("Data/Corsica_landCover.tif"))
rd<-rast("Data/Corsica_roadDistance.tif")

#make corsica masked raster
mask<-lc==0
r<-mask(x=c(lc, e, s, rd), mask=mask, maskvalues=1, updatevalue=NA )
r<-project(r, "EPSG:31467")

#########################
#check rasters
ggplot()+
    geom_spatraster(data=r, aes(fill=landcover))+
    theme_minimal()

ggplot()+
    geom_spatraster(data=r, aes(fill=elevation))+
    theme_minimal()


ggplot()+
    geom_spatraster(data=r, aes(fill=slope))+
    theme_minimal()

values(r$landcover) %>% table

ggplot()+
    geom_spatraster(data=r, aes(fill=distance))+
    theme_minimal()

#################
#output
terra::writeRaster(r, "Outputs/CombinedRaster.tif", overwrite=TRUE)