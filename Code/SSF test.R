install.packages("enerscape")
library(enerscape)
?enerscape
library(move2)
library(move)
library(adehabitatLT)
library(sf)
library(lubridate)
library(circular)
library(ggplot2)
library(rnaturalearth)
library(units)
library(dplyr)
library(purrr)
library(tidyr)
library(scales)



## SSF test : 
?step
#step(lm(~ASTER_elevation, data=cervus_clean), direction="both") 
cervus_env<-mt_read(file = "cervus elaphus corsicanus_env.csv")
colnames(cervus_env)[colnames(cervus_env) == 'ASTER Elevation'] <- 'ASTER_elevation'
colnames(cervus_env)[colnames(cervus_env) == 'individual-local-identifier'] <- 'individual_local_identifier'

step(lm(cervus_env$ASTER_elevation~file$location.long), direction="both")
### ???? 


#########################################################
###               Trajectory Analysis                 ###
#########################################################
plot<-ggplot() + theme_bw() +
  geom_sf(data = rnaturalearth::ne_countries(country="France")) +
  geom_sf(data = mt_track_lines(cervus_clean), color="firebrick")+
  coord_sf(xlim=c(min(sf::st_coordinates(cervus_clean)[,1]),max(sf::st_coordinates(cervus_clean)[,1])), ylim=c(min(sf::st_coordinates(cervus_clean)[,2]),max(sf::st_coordinates(cervus_clean)[,2])))

tapply(mt_time(cervus_clean), list(year(mt_time(cervus_clean)), month(mt_time(cervus_clean))), length)
table(year(mt_time(cervus_clean)), month(mt_time(cervus_clean)))

cervus <- cervus_clean %>% mutate(azimuth = mt_azimuth(cervus_clean), 
                      speed = mt_speed(cervus_clean), 
                      turnangle = mt_turnangle(cervus_clean))

###________________________________________________
### Azimuth distribution with speed and season ####

### Categorize seasons. Numbers refer to month.
# assign the categories to a new variable based on the timestamp
# assign season = NA to segments where subsequent locations fall in different seasons
cervus  <- mutate(cervus,
               month = month(mt_time(cervus)),
               season = recode_factor(month,
                                      "1"="Breeding", "2"="Breeding",
                                      "3"="Breeding", "4"="Not breeding",
                                      "5"="Not breeding", "6"="Not breeding",
                                      "7"="Not breeding", "8"="Not breeding",
                                      "9"="Not breeding", "10"="Breeding",
                                      "11"="Breeding", "12"="Breeding"),
               season = if_else(season == lead(season, 1) &
                                  mt_track_id(cervus) == lead(mt_track_id(cervus), 1),
                                season, NA)
)

### Look at direction of movement when movement is happening, that is:
# select segments above 100 m/h, we are only interested in segments when Leo is moving, and not the stationary error
# remove missing values in season and group data by season
cervus_moving <- filter(cervus, speed > set_units(100, "m/h") & !is.na(season)) %>% group_by(season)

### Speed ~ azimuth scatter plot
# speed here is ground speed (how fast is the animal in making way)
ggplot(cervus_moving, aes(x = azimuth, y = speed)) +
  geom_point(color=alpha("black", 0.3)) +
  scale_x_units(unit = "degrees", breaks = c(-2:2) * 90, expand = c(0L, 0L)) +
  scale_y_units(unit = "m/h") +
  theme_linedraw()
# Note to plot:
# -high speeds predominantly occur when azimuths are around roughly -180-180∘ and 0∘ (N and S), which are the result of Leo’s migratory behaviour.


### Azimuth density per season
# store the information in a new data frame
cervus2 <- data.frame(D=cervus_moving$azimuth,
                 V=cervus_moving$speed,
                 Season=cervus_moving$season)
head(cervus2)
# Define the direction as a circular object
cervus2$Dcirc <- as.circular(drop_units(cervus2$D), # this function does not "understand" units
                        rotation="clock",
                        units="radians",
                        type="angles",
                        modulo="asis",
                        zero=0,
                        template="geographic")

# remove missing values
cervus2 <- cervus2[complete.cases(cervus2),]
# vector with the order of the factor levels, used to set the order of plotting
seasons <- levels(cervus_moving$season)
# store margin defaults before changing them
parDefaults <- par()
par(mar=rep(1,4))
# plot all the azimuths (circular histogram)
plot(cervus2$Dcirc, stack=T, shrink=1.6, pch=16, sep=0.05, col="grey")
# loop through seasons and plot a line density per season
for(i in 1:length(seasons)){
  # subset the azimuth
  x <- cervus2[cervus2$Season==seasons[i],'Dcirc']
  # calculate density and plot as a line
  lines(density(x, bw=180, kernel="vonmises"), lwd=2, lty=i)
  # draw an arrow showing mean and resultant length
  arrows.circular(mean(x), y=rho.circular(x), lwd=2, length=0.1, lty=i)
}
# add a legend
legend("bottomleft", lty=c(1,2,3,4), seasons, bty="n", cex=0.85)
# Note to plot (only contains directional information):
# - grey histogram: number of locations
# - lines: circular density function
# - arrows: mean direction; arrow length: mean resultant length, measure of variance of the data (wintering and breeding can be barely seen)
# - wintering might need better classification
par(parDefaults)

### Wind rose, similar plot but showing azimuth AND speed, again per season
cervus_gg <- mutate(cervus_moving, speed_categorical = cut(set_units(speed, m/h), breaks = c(2, 5, 10, 15, 35)))
cervus_gg <- filter(cervus_gg, !is.na(season))

ggplot(cervus_gg) +
  coord_polar(start = pi) +
  geom_histogram(
    aes(x = set_units(azimuth, "degrees"),fill = speed_categorical),
    breaks = set_units(seq(-180L, 180L, by = 10L), "degrees"),
    position = position_stack(reverse = TRUE)) +
  scale_x_units(
    name = NULL,
    limits = set_units(c(-180L, 180L), "degrees"),
    breaks = (-2L:1L) * 90L,
    labels = c("S", "W", "N", "E")) +
  scale_y_continuous(name = NULL, limits = c(-1, 120), expand = c(0L, 0L)) +
  facet_wrap(~season) +
  scale_fill_ordinal("Speed [m/h]") +
  theme_linedraw()
# Note to plot:
# - migration has directionality with high speeds
# - breeding has only low speeds
# - wintering seems to contain some migration. But we just took "month: to categorize.

#__________________________________________________
## CORRELATION IN DIRECTION & MOVEMENT PROCESS ####
#__________________________________________________
### Simulate 500 tracks with 1000 steps, 
### with different levels of correlation of azimuth (0.8 <= r <= 0.995)
set.seed(1512)
?set.seed
library(adehabitatLT)
?sort
?lapply
rVals <- sort(rep((0.8 + log10(c(seq(1,100, length.out=10)))/10)[1:9], each=500))
rCRW <- lapply(lapply(rVals, simm.crw, date=1:1000, h=1), as, "Move") #convert to move
# calculate NSD for all tracks, from origin of trajectory
rNSD <- unlist(lapply(lapply(lapply(rCRW, coordinates), spDistsN1, pt=c(0,0)), "^", 2))
# calculate the mean NSD of all 500 trajectories of each value of r
mNSD <- tapply(rNSD, list(sort(rep(rVals,1000)), rep(1:1000, length(rVals))), mean)

### Plot NSD as a function of increasing number of steps for different correlations (r) in directions:
par(mar=c(5, 4, 4, 4) + 0.1)
plot(0,0, type="n", xlim=c(0,1300), ylim=c(0, max(mNSD)),
     bty="n", xlab="Step", ylab="Net square distance", xaxt="n")
axis(1, at=c(0,200,400,600,800,1000))
test <- apply(as.matrix(mNSD), 1, lines, x=1:1000)
text(cbind(rep(c(1250, 1100), length.out=length(row.names(mNSD))), mNSD[,ncol(mNSD)]),
     paste("r=", as.character(round(as.numeric(row.names(mNSD)),3)),sep=""), cex=0.5)
# Note to plot:
# - slope: how fast it gets away from the origin 

# group trajectories by correlation values
rCRW_ls <- split(rCRW, round(rVals, 3))
par(mfrow=c(3,3))
lapply(rCRW_ls[["0.8"]][sample(1:500, 3)], plot, pch=19, main="r = 0.8")
lapply(rCRW_ls[["0.975"]][sample(1:500, 3)], plot, pch=19, main="r = 0.975")
lapply(rCRW_ls[["0.995"]][sample(1:500, 3)], plot, pch=19, main="r = 0.995")
# Note to plot:
# - high r = very directional movement, steep increase in NSD per step made, gets away fast from origin
# - low r = movement in all direction around the origin, wiggly (brownian motion), takes long time to get away



#____________________
# TRACK ANALYSIS ####
#____________________
# Annotate speed, azimuth and turning angle to the trajectory
cervus_ta <- mutate(cervus_clean, speed = set_units(mt_speed(cervus_clean),m/h), 
               timelag=set_units(mt_time_lags(cervus_clean),day), 
               distance=set_units(mt_distance(cervus_clean),m))
# add column sex to event attribute
cervus_ta$`individual-local-identifier`<- as.character(cervus_ta$`individual-local-identifier`)

cervus_ta$sex <- ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Barbara"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Sara"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Antonia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Giulia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Sabrina"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Victoria"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Vanina"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Aurelia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Mattea"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Lia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Violetta"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Luvana"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Sapara"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Latonaccia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Lama"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Chisaccia"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Cavallara"), "F",
                 ifelse(endsWith(cervus_ta$`individual-local-identifier`, "Stella"), "F","M"))))))))))))))))))

#cervus_ta <- subset( cervus_ta, select = -`ifelse(Barbara == T, "F", "M")`)
cervus_ta <- mt_as_event_attribute(cervus_ta,sex)## don't run but following code works anyway ???

#check id column and summarize values per id
mt_track_id_column(cervus_ta)
#bats <- group_by_track_data(bats, individual_local_identifier) # this works in the github version, or 
cervus_ta <- group_by(cervus_ta, `individual-local-identifier`)
cervusPerTrack <- summarize(cervus_ta,
                          sex=unique(sex),
                          medianSpeed=median(speed,na.rm=T),
                          timeTracked=sum(timelag,na.rm=T),
                          distanceTracked=sum(distance,na.rm=T)) #cumulative distance tracked
indData <- st_drop_geometry(cervusPerTrack)
head(indData, 4)

## Test for differences in distance traveled per day between sexes
indData <- droplevels(indData)
boxplot(log10(I(distanceTracked/timeTracked)) ~ sex, data=indData, names=c("Males", "Females"),
        ylab=expression(paste(Log_10, " of cumulative distance in m per day", sep="")))
# Note to plot: there may be a small difference
indDataNoUnits <- drop_units(indData)
t.test(log(I((distanceTracked/1000))/timeTracked) ~ sex, data=indDataNoUnits)
# but this difference does not seem to be significant
# with a glm we can add co-variates
mod <- glm(sqrt(distanceTracked) ~ as.factor(sex) + timeTracked, data=indDataNoUnits)
# we check that assumptions are met
par(mfrow=c(2,2))
plot(mod, ask=F)
# and we see that the longer we track them, the higher the cumulative distance (of course)
summary(mod)



### Test for differences in speed between sexes
par(mfrow=c(1,1))
boxplot(log(medianSpeed) ~ sex, data=indDataNoUnits, names=c("Males", "Females"), ylab="Median speed in m/h")
# Note to plot: males seem to travel faster than females.. one obs with interesting negative speed, but we leave it for now
wilcox.test(medianSpeed ~ sex, data=indDataNoUnits)
# still no significant differences ....


# Speeds and distances have naturally very long tails, and to run linear models we often have to transform our data
# Usually log or sqrt, but if we are not sure, boxcox is a nice function to find a right transformation of the data
# If we minimise the bias in the residuals the estimated slope is correct
bc <- boxcox(medianSpeed ~ as.factor(sex) + timeTracked, data=indDataNoUnits[-15,])
modII <- glm(I(medianSpeed^bc$x[which.max(bc$y)]) ~ as.factor(sex)+timeTracked, data=indDataNoUnits[c(-15),])
par(mfrow=c(2,2))
plot(modII)
summary(modII)
# there is a difference between sexes in speed
# time tracking is not significant anymore, because is integrated in the speed calculation
# given what we said about the effect of sampling frequency we could check that too
cervus_ta$timelag_min <- set_units(cervus_ta$timelag, min) 
group_by(cervus_ta, `individual-local-identifier`, sex) %>% 
  summarize(medianTimelag=median(timelag_min, na.rm=T))



