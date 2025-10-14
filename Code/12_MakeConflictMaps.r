library(tidyverse)
library(tidyterra)
library(terra)

# -----------------------------
# load rasters
# -----------------------------
r<-rast("Outputs/MeanSim.tif")[[5]]-rast("Outputs/MeanSim.tif")[[4]]
lc <- rast("Outputs/Corsica_WorldCover_v100_2025.tif")






# -----------------------------
#process the deer movement raster
# -----------------------------
patch_raster <- patches(r, zeroAsNA = TRUE, directions=4)


# Convert patches to polygons
patch_polygons <- as.polygons(patch_raster)

# Crop raster to each patch
cropped_rasters <- lapply(1:length(patch_polygons), function(i) {
  crop(r, patch_polygons[i,])
})


# Function to classify and polygonize a single cropped raster
classify_and_polygonize <- function(cropped_rast) {
  # Reclassify the cropped raster based on quantiles
  classified_rast <- terra::classify(cropped_rast, rcl = matrix(
    c(0, 1, 0,
        1, 10, 1,
      10,100, 2,
      100, 1000, 3,
      1000, 10001, 4),
      ncol = 3, byrow = TRUE
  ))

  # Convert the classified raster to polygons
  polygons <- as.polygons(classified_rast)
  return(polygons)
}

# Apply the function to each cropped raster
cropped_polygons <- lapply(cropped_rasters, function(x) classify_and_polygonize(x))

# You can now work with the list of polygon objects (cropped_polygons)
# For example, to see the first set of polygons:
plot(cropped_polygons[[1]])


# Get the spatial extent (bounding box) of the SpatVector
e <- ext(cropped_polygons[[1]])

# Extract the minimum and maximum latitudes (y-coordinates)
min_lat <- e$ymin
max_lat <- e$ymax

# Calculate the halfway latitude
halfway_lat <- (min_lat + max_lat) / 2


e_north <- ext(e$xmin, e$xmax, halfway_lat, e$ymax)
e_south <- ext(e$xmin, e$xmax, e$ymin, halfway_lat)

a<-crop(cropped_polygons[[1]], e_north)
b<-crop(cropped_polygons[[1]], e_south)

cropped_polygons[[1]]<-a
cropped_polygons[[3]]<-b





# -----------------------------
#process the land cover raster
# -----------------------------
# Define the reclassification matrix for ESA WorldCover codes
# Mapping ESA codes to new categorical values (e.g., 1 to 10)
rcl_esa <- matrix(
  c(10, 1,  # Tree cover -> 1
    20, 2,  # Shrubland -> 2
    30, 3,  # Grassland -> 3
    40, 4,  # Cropland -> 4
    50, 5,  # Built-up -> 5
    60, 6,  # Bare / sparse vegetation -> 6
    70, 7,  # Snow and ice -> 7
    80, 8,  # Permanent water bodies -> 8
    90, 9,  # Herbaceous wetland -> 9
    95, 10, # Mangroves -> 10
    100, 11 # Moss and lichen -> 11
    ),
  ncol = 2, byrow = TRUE
)

landcover_names <- c(
    NA,
  "Tree cover",
  "Shrubland",
  "Grassland",
  "Cropland",
  "Built-up",
  "Bare / sparse vegetation",
  "Snow and ice",
  "Permanent water bodies",
  "Herbaceous wetland",
  "Mangroves",
  "Moss and lichen"
)

# Reclassify the land cover raster
lc_categorical <- terra::classify(lc, rcl_esa, right = NA)

levels(lc_categorical) <- data.frame(id = 0:11, name = landcover_names)


# Display the categorical raster (optional)
plot(lc_categorical)





# -----------------------------
# make a results table
# -----------------------------
makeLandCoverTable<-function(p) {

    extracted_values<-extract(lc_categorical, p)

    tbl<-extracted_values %>%
        group_by(ID) %>%
        count(name) %>%
        mutate(n=signif(n/(100*100), 2)) %>%
        pivot_wider(names_from=name, values_from=n) %>%
        rename(Visits=ID)
    tbl$Visits<-c("0-1", "1-10", "10-100", "100-1000", "1000-10000")

    return(tbl)
}

table_l<-lapply(cropped_polygons, makeLandCoverTable)

names(table_l)<-c("North", "South", "Center")

for (region in names(table_l)){
    write.csv(table_l[[region]], paste0("Outputs/ConflictTable",region, ".csv"))
}


# -----------------------------
# make the plot
# -----------------------------
# Define the standard ESA WorldCover color palette
# These are approximate colors based on the ESA WorldCover legend
esa_palette <- c(
  "#0032c8", # NA
  "#006400", # 10 Tree cover
  "#ffbb22", # 20 Shrubland
  "#ffff4c", # 30 Grassland
  "#f096ff", # 40 Cropland
  "#fa0000", # 50 Built-up
  "#b4b4b4", # 60 Bare / sparse vegetation
  "#f0f0f0", # 70 Snow and ice
  "#0032c8", # 80 Permanent water bodies
  "#0096a0", # 90 Herbaceous wetland
  "#00cf75", # 95 Mangroves
  "#fae6a0"  # 100 Moss and lichen
)

esa_named_palette <- setNames(esa_palette, landcover_names)

# Function to create a map for a single set of polygons
create_map <- function(polygons, landcover_raster) {

    polygons <- terra::project(polygons, crs(landcover_raster))
    cropped_raster<-crop(landcover_raster, polygons)

  ggplot() +
    geom_spatraster(data =cropped_raster) + # Plot the land cover raster
    scale_fill_manual(values = esa_named_palette, name = "Land Cover", na.value= "#0032c8" ) + # Apply ESA palette to land cover
    geom_spatvector(data = polygons, fill = NA, color="black", aes(linewidth=`18264`)) + # Plot the polygons using geom_spatvector
    scale_linewidth("# visits", labels=c("0-1", "1-10", "10-100", "100-1000", "1000-10000"), range = c(0.25, 2))+
    theme_minimal()+
    xlab("")+ylab("")
}

# Create maps for each set of cropped polygons
# This will generate a list of ggplot objects, one for each cropped patch
maps <- lapply(cropped_polygons, function(x) create_map(x, lc_categorical)) # Use lc_categorical here

# You can display the first map as an example
north<-maps[[1]] +   geom_label(
    aes(x = 9.23, y = 42.45), # Coordinates for the annotation
    label = "Ponte Leccia",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) +   geom_label(
    aes(x = 8.97, y = 42.62), # Coordinates for the annotation
    label = "L'ile Rouse",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) + theme(legend.position="none")


center<-maps[[3]]  +   geom_label(
    aes(x = 9.13, y = 42.31), # Coordinates for the annotation
    label = "Corte",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) +   geom_label(
    aes(x = 9.2, y = 42.11), # Coordinates for the annotation
    label = "Ghisoni",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) + theme(legend.position="none")


south<-maps[[2]]  +   geom_label(
    aes(x = 9.32, y = 42), # Coordinates for the annotation
    label = "Ghisonaccia",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) +   geom_label(
    aes(x = 9.31, y = 41.74), # Coordinates for the annotation
    label = "Conca",
    color = "black",
    size = 4,
    fontface = "bold",
    fill="white"
  ) + theme(legend.position="none")



legend_plot <- maps[[2]] + 
  theme(legend.position = "bottom") 

# Extract the horizontal legend as a separate object
legend <- cowplot::get_plot_component(legend_plot, "guide-box-bottom")

mapsPlot<-cowplot::plot_grid(north, center, south , labels = c('North', 'Center', "South"), label_size = 12, ncol=3, rel_widths=c(1,1, 0.85))

cowplot::plot_grid(
  mapsPlot, 
  legend, 
  ncol = 1, 
  rel_heights = c(1, 0.1) # Assigns 10% of the space to the legend row
)
ggsave("Figures/ConflictMaps.png", bg="white", height=8.5, width=21)

