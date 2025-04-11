
#######################
# some basic plots to check that the model makes sense and extraction of a few key values
#####################

library(tidyverse)
library(tidyterra)
library(terra)
library(ggspatial)

#load
df<-readRDS("Outputs/ModelDataframe.RDS")
r<-terra::rast("Outputs/CombinedRaster.tif")

# visualise deer areas
points<-df %>% select(id, x1_, y1_) %>% rename(lat=y1_, lon=x1_) %>% tidyterra::as_spatvector()
crs(points)<-"EPSG:31467"
hulls<-convHull(points, by="id")
sites<-aggregate(hulls) %>% disagg

siteMaps<-list()
for (site in 1:length(sites)) {
    siteMaps[[site]]<- ggplot() +
        geom_spatraster(data=crop(r, ext(sites[site])), aes(fill=landcover))+
        geom_spatvector(data= crop(hulls, ext(sites[site])), color="black", linetype=2,  fill="transparent")+
        scale_fill_manual(values = c("orangered", "yellow3", "springgreen4", "olivedrab", "sandybrown"))+
        ggspatial::annotation_scale(
            height = unit(0.015, "npc"),
            width_hint = 0.5,
            pad_x = unit(0.07, "npc"),
            pad_y = unit(0.07, "npc"),
            text_cex = .8
        )
    ggsave(file.path("Figures", paste0("DeerLandUse_", site, ".png")), siteMaps[[site]])
}


#comapre deer habitat use to that of randomised deer
LandClassUsePlot<-df %>% select(id, case_, landcover) %>%
    group_by(id, case_, landcover) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    pivot_wider(names_from=case_, values_from=n) %>%
    mutate(useRate=`TRUE`/`FALSE`) %>%
    ggplot(data=., aes(x=landcover, y=useRate, color=landcover))+
        geom_boxplot(outlier.shape=NA)+
        geom_jitter(width=0.2, size=2)+
        scale_color_manual(values = c("orangered", "yellow3", "springgreen4", "olivedrab", "sandybrown"))+
        theme_classic()+
        ylab("Proportion of time spent by a single deer")
ggsave("Figures/LandClassUseRate.pdf", LandClassUsePlot)
#check nas in winter females

df %>% names
df %>% select(sex, season,sl_) %>%  summary

df %>%
    ggplot()+
        geom_boxplot(aes(x=log(sl_), y=season, color=sex))

#check project length
interval(first(df$t1_), last(df$t2_)) /years(1)



# Considerations for max step length in ABM
max(df$sl_)
summary(df$sl_)

# exclude x% longest steps?
n=length(df$sl_)
sort(df$sl_)[floor(n*0.9999):n]

# look at stats of excluded step options
summary(sort(df$sl_)[(floor(n*0.9999)-2):n])
summary(sort(df$sl_)[(floor(n*0.999)-1):n])

# explore computational implications
getPixelsConsideredPerStep<-function(maxStep){ (maxStep^2*pi)/(100*100)}
getPixelsConsideredPerStep(8650)
getPixelsConsideredPerStep(4000)
getPixelsConsideredPerStep(2600)


# explore computational implications of sampling the max step length from the observed distribution
# for each deer at each step
randomisedMaxLengths<-sample(x=df$sl_, replace=TRUE, size=1000000)
sum(getPixelsConsideredPerStep(randomisedMaxLengths))/length(randomisedMaxLengths)
