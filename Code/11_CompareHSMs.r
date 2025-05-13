#######################
# compare the habitat selection models from the real and simulaed data
#####################

library(tidyverse)
library(tidyterra)
library(terra)

#1) load
ascs<-( list.files("Outputs/DeerSimVisitMaps", full=TRUE))
r<-terra::rast("Outputs/CombinedRaster.tif")
bursts<-readRDS("Outputs/bursts.RDS")

#2) get simulated visits
# create list of rasters per sim in order of steps completed
r_l<-lapply(
    split(ascs, ceiling(seq_along(ascs)/5)), 
    function(item){ 
        df<-data.frame("file"=item, "step"=as.numeric(str_sub(item, 48, -5))) %>%
            arrange(step)
        r<-df %>%
            pull(file) %>%
            rast()
        names(r)<- pull(df, step)
        return(r)
    }
)
#align to raster
r_l_aln<-lapply( r_l, 
    function(item) {
        simr<-terra::project(item, r)
        return(simr)
    })
# get average visits in 2025 across sims
simr<-lapply( r_l_aln, `[[`, 2) %>% 
    rast %>%
    mean
#mask 0s
simr[simr==0] <- NA
#standardise to 0-1
simr<-simr/max(values(simr)  , na.rm=TRUE)  


# 3) get real deer visit raster, standaised to 0-1
VisitDensity<-lapply( bursts$data, function(item){ vect(x=as.data.frame(item), crs=crs(r), geom=c("x_", "y_"))}) %>% 
    vect() %>%
    rasterize(., r, fun=length)
VisitDensity<-VisitDensity/max(values(VisitDensity)  , na.rm=TRUE)  

# 4) get corsica outline
corsica<- (patches(r$elevation)==1) %>% as.polygons


# 5) make model parameter plots
p1<-ggplot() +
    geom_spatraster(data=VisitDensity, aes(fill=V1))+
    geom_spatvector(data=corsica, fill=NA)+
    viridis::scale_fill_viridis(na.value = "transparent")+
    theme_bw()+
    facet_wrap(~"real")+
    guides(fill="none")

p2<-ggplot() +
    geom_spatraster(data=simr, aes(fill=mean))+
    geom_spatvector(data=corsica, fill=NA)+
    viridis::scale_fill_viridis("Visit rate", na.value = "transparent")+
    theme_bw()+
    facet_wrap(~"sim")


# model 
predicting_df<-as.data.frame(r) %>% filter(complete.cases(.)) 


real_df<-as.data.frame(c(VisitDensity, r)) %>% filter(complete.cases(.))
real_rf<-ranger::ranger(V1~. , real_df, importance="impurity")
ranger::importance(real_rf)
pred_df<-cbind(predicting_df, "pred"=predict(real_rf, predicting_df)$predictions) %>%
    as_tibble %>%
    mutate(`distance (km)`=distance/1000) %>%
    mutate(`elevation (100m)`=elevation/100) %>%
    pivot_longer(c(`elevation (100m)`, slope, `distance (km)`))

sim_df<-as.data.frame(c(simr, r)) %>% filter(complete.cases(.))
sim_rf<-ranger::ranger(mean~. , sim_df, importance="impurity")
ranger::importance(sim_rf)
predsim_df<-cbind(predicting_df, "pred"=predict(sim_rf, predicting_df)$predictions) %>%
    as_tibble %>%
    mutate(`distance (km)`=distance/1000) %>%
    mutate(`elevation (100m)`=elevation/100) %>%
    pivot_longer(c(`elevation (100m)`, slope, `distance (km)`))

plot_df<-rbind(cbind(pred_df, "type"="real"), cbind(predsim_df,  "type"="sim")) %>% as_tibble

p3<-ggplot(plot_df)+
    geom_hex(aes(y=value, x=(pred)), bins=30)+
    viridis::scale_fill_viridis()+
    facet_grid(type~name, )+
    theme_bw()+
    guides(fill="none")+
    xlab("Predicted visit rate")+ylab("")

ggplot(plot_df)+
    geom_density(aes(x=(pred)), fill="grey")+
    facet_grid(type~landcover)+
    theme_bw()



toprow<-cowplot::plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12, rel_widths=c(1, 1.15))

cowplot::plot_grid(toprow, p3, labels = c('', 'C'), label_size = 12, ncol = 1, rel_heights=c(2.5, 1))

ggsave("Figures/VisitMapModels.png", bg="white", height=12, width=10)




# 6) make habitat suitability map plots

pfun <- \(...) {
    predict(...)$predictions
}

pr1 <- predict(r, real_rf,  fun=pfun, na.rm = TRUE)
pr2 <- predict(r, sim_rf,  fun=pfun, na.rm = TRUE)

m1<-ggplot() +
    geom_spatraster(data=pr1, aes(fill=lyr1))+
    theme_bw()+
    facet_wrap(~"real")+
    viridis::scale_fill_viridis(na.value = "transparent",  limits=c(0,1))+
    guides(fill="none")

m2<-ggplot() +
    geom_spatraster(data=pr2, aes(fill=lyr1))+
    theme_bw()+
    facet_wrap(~"sim")+
    viridis::scale_fill_viridis("Habitat\nsuitability", na.value = "transparent",  limits=c(0,1))

cowplot::plot_grid(m1, m2, labels = c('A', 'B'), label_size = 12, rel_widths=c(1, 1.15))
ggsave("Figures/VisitMapHSMs.png", bg="white", height=8, width=10)
