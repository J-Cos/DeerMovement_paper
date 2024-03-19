install.packages("amt")
install.packages("tidyverse")
install.packages("lubridate")
library(amt)
library(tidyverse)
library(lubridate)
library(sf)
library(arulesViz)

#cervus_test<-(amt::make_track(cervus_clean, .x=x,.y=y,.t=t,
#                              crs = sp::CRS("+init=epsg:4326"))%>%
#                               amt::transform_coords(sp::CRS("init=epsg:4171"))
              

## create x and y columns 
cervus_xyt <- st_sf('geometry' = cervus_clean$geometry, 
                     'timestamp' = cervus_clean$timestamp)
library(magrittr)
?st_sf
?st_coordinates
cervus_xyt<- st_sf((cervus_xyt))
cervus_xyt<-cbind(cervus_xyt, st_coordinates(cervus_xyt))
class(cervus_xyt)
#cervus_track <- make_track(cervus_xyt,"x_", "y_","t_")
#cervus_track <- make_track(cervus_xyt, x, y, t)
df1 <- tibble(x =cervus_xyt$X, y =cervus_xyt$Y, t = cervus_xyt$timestamp)
cervus_xyt <- make_track(df1, .x=x, .y=y, .t=t,crs=4326)
class(cervus_xyt)
amt::summarize_sampling_rate(cervus_xyt)
#mean = 4962s = 1.3783h

stps<-amt::track_resample(cervus_xyt, rate=minutes(10),
                          tolerance=seconds(60))
str(stps)


##########   VIDEO YOUTUBE ############
# I used a video to help me with the following lines
#i Just like to precise where i found the code before using it 
cervus_xyt<-mutate(ymd_hms(cervus_xyt$t_))%>%
  make_track(x_,y_,t_)


ggplot()+geom_sf(data=cervus_ta)+
  geom_point(aes(x_,y_),data=cervus_xyt)

amt::summarize_sampling_rate(cervus_xyt)
stps<-amt::track_resample(cervus_xyt, rate=minutes(1),
                          tolerance=seconds(60))

ggplot()+geom_sf(data=cervus_ta)+
  geom_point(aes(x_,y_),data=stps)
stps

stps2<-stps%>%
  filter_min_n_burst(min_n = 3)%>%
  steps_by_burst()%>%
  time_of_day(include.crepuscule = FALSE)
stps2

### create the raster : 
dem_france2<-raster("France.tif")
plot(dem_france)
crop<-extent(4200000,4300000,10,2218000)
demfrance_crop2<-crop(dem_france2, crop)
plot(demfrance_crop2)
plot(demfrance_crop)
class(demfrance_crop2)
demfrance_crop2

cropbox2 <- drawExtent()
?drawExtent
#crop the raster, then plot the new cropped raster
dem_crop_1 <- crop(demfrance_crop2, cropbox2)
#plot the cropped extent
plot(dem_crop_1)
#crs(dem_crop) <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"
dem_crop_1

#create the steps using the article :
library(survival)
#the following code is exaclty the one found in the article
#It doesn't work for me. 
m1<-stps2%>% amt::random_steps(n=9)%>%
  amt::extract_covariates(stps2, eurodem_crop2)%>%
  amt::time_of_day(m1, include.crepuscule=FALSE)%>%
  mutate(log_sl_=log(sl_))%>%
  amt::fit_issf(case_~eurodem_crop2+log_sl_ +eurodem_crop2:tod_end_+log_sl_:tod_end_
                  +strata(step_id_), data=m1)
#BUT, if I seperate everything it kinda works : 
#The individual lines works but it doesn't create correclty the m1 data frame
#It misses some columns like sl_ and/or step_id_....
m1<-stps2%>% amt::random_steps(n=9)
m1>-amt::extract_covariates(stps2, demfrance_crop2)
m1<-amt::time_of_day(m1, include.crepuscule=FALSE)
m1<-mutate(log_sl_=log(sl_), data=m1)
m1<-amt::fit_issf(formula=case_~demfrance_crop2+log_sl_ +demfrance_crop2:tod_end_+log_sl_:tod_end_
                  +strata(step_id_), data=m1)


formula(m1)
??case_
?time_of_day
class(m1$sl_)
?mutate
class(m1)
