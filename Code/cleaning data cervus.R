### imoport data directly from Movebank : 
library(move)
library(raster)

#### Import data set from MoveBank (Buffalo test)
cred<-movebankLogin(username="Hannah.S",password="Morphee1")
searchMovebankStudies(x="Kruger African Buffalo", login=cred)
getMovebankStudy("Kruger African Buffalo, GPS tracking, South Africa", login=cred)
buffalo<-getMovebankData("Kruger African Buffalo, GPS tracking, South Africa", login=cred)
buffalo
####   Reading in a .csv file downloaded from MOveBank ###
#buffalo<-move("file name")
#buffalo
#### from a csv file : 
buffalo_csv<- move("Kruger African Buffalo, GPS tracking, South Africa.csv.gz")
buffalo(csv)

#### Import data set from MoveBank (Cerf Corse)
cred<-movebankLogin(username="Hannah.S",password="Morphee1")
searchMovebankStudies(x="cervus elaphus corsicanus", login=cred)
getMovebankStudy("cervus elaphus corsicanus", login=cred)
cervus<-getMovebankData(study="cervus elaphus corsicanus", login=cred)
cervus

cervus.csv<-read.csv("cervus elaphus corsicanus.csv")

###---------------------------------------------###
###  Creating a move object from any data set   ###
###---------------------------------------------###
# read the data and store in a data frame
file <- read.csv("cervus elaphus corsicanus.csv", as.is=T)
str(file)

# first make sure the date/time is in POSIXct format
file$timestamp <- as.POSIXct(file$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC")

# also ensure that timestamps and individuals are ordered
file <- file[order(file$individual.local.identifier, file$timestamp),]

# convert a data frame into a move object
cervus <- move(x=file$location.long,y=file$location.lat,
             time=file$timestamp, #already in POSIXct format
             data=file, proj=crs("+proj=longlat +ellps=WGS84"),
             animal=file$individual.local.identifier, sensor="gps")


###---------------------------------------------###
###                  MOVE 2                     ###
###---------------------------------------------###

install.packages("move2")
library(move2)
??move2

library(dplyr)
library(sf)
library(readr)

cred2<-movebank_store_credentials("Hannah.S","Morphee1")
movebank_get_study_id("cervus elaphus corsicanus")
movebank_download_study_info(study_id = 1926018864)%>%
  print(width = Inf) #all cols
#searchMovebankStudies("cervus elaphus corsicanus", cred2)
#movebank_download_study_info("cervus elaphus corsicanus", login=cred2)
cervus2<-	movebank_download_study(study_id=1926018864)
cervus2

# from .csv file :
cervus.csv <- mt_read("cervus elaphus corsicanus.csv")
str(cervus.csv)

# When downloading data using movebank_download_study(), the argument
#"remove_movebank_outliers" is TRUE by default
mt_movebank_visible(cervus.csv) %>% table()
# but if false, we could then remove them by:
#cervus2 <- mt_filter_movebank_visible(cervus2)

### EMPTY COORDINATES (in case of missed fixes or other sensors as ACC)
# Empty coordinates are allowed in move2, if we want to remove them we can:
sf::st_is_empty(cervus.csv) %>% table()
cervus.csv <- dplyr::filter(cervus.csv, !sf::st_is_empty(cervus.csv)) # Omit empty locations

### DUPLICATES
### By default, duplicated timestamps are allowed and kept in the dataset when downloading from Movebank
# But there are different ways to remove them

table(duplicated(mt_time(cervus.csv)))
# remove duplicated times without control of which locations are being removed (not recommended usually)
cervus_NoDups <- mt_filter_unique(cervus.csv, criterion = "first") #or sample
table(duplicated(mt_time(cervus_NoDups)))
# often duplicates are caused by the same location transferred twice via different sources, once with more information than the other 
# add criterion="subsets" to remove duplicates that are subset of others (recommended)
cervus_NoDups2 <- mt_filter_unique(cervus.csv, criterion = "subsets") 
table(duplicated(mt_time(cervus_NoDups2)))

# select the timestamps corresponding to duplicated events
tdup <- mt_time(cervus.csv)[duplicated(mt_time(cervus.csv))]
# and inspect the first duplicate
dups <- filter(cervus.csv, mt_time(cervus.csv)==tdup[1])
print(dups, width = Inf)

# some have different values, so we need to exclude them (note the use of mt_unique instead of mt_filter_unique):
cervus_NoDups <- cervus.csv[mt_unique(dplyr::select(cervus.csv, 
                                          -c("event-id", "individual-local-identifier"))), ]
table(duplicated(mt_time(cervus_NoDups)))

#______________________
# OUTPUTTING DATA ####
#______________________

### Save the move2 object as RData file
save(cervus_NoDups, file="cervus_clean.Rdata")
load("cervus_clean.Rdata") #cannot be assigned a new name, but can contain multiple objects

### Or as an rds file
#saveRDS(srk_noDups, file="SieritStork_cleaned.rds")
#sierit <- readRDS("SieritStork_cleaned.rds") # can only contain one object, but you can give it a name when loading


### Save as csv file
# at the moment with an extra step to deal with the geometry, but this will be solved soon
cervus_noGeom <- st_drop_geometry(cervus_NoDups) %>% as.data.frame()
coords <- as.data.frame(st_coordinates(cervus_NoDups)) %>% as.data.frame() %>% 
  rename("location-long" = "X", "location-lat" = "Y")

cervusDF <- cbind(cervus_noGeom, coords)
head(cervusDF)

write.table(cervusDF, file="cervus_clean.csv", sep=",", row.names = FALSE)
cervus_clean<-mt_read("cervus_clean.csv")

#colnames(cervus2)[colnames(cervus2) == 'individual_local_identifier'] <- 'individual-local-identifier'
#colnames(cervus_NoDups)[colnames(cervus_NoDups) == 'individual_local_identifier'] <- 'individual-local-identifier'
#colnames(cervus_NoDups2)[colnames(cervus_NoDups2) == 'individual_local_identifier'] <- 'individual-local-identifier'
#colnames(cervus_noGeom)[colnames(cervus_noGeom) == 'individual_local_identifier'] <- 'individual-local-identifier'
#colnames(cervusDF)[colnames(cervusDF) == 'individual_local_identifier'] <- 'individual-local-identifier'
#colnames(cervus.csv)[colnames(cervus.csv) == 'ASTER Elevation'] <- 'ASTER_elevation'





