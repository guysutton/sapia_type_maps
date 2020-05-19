# Example functionality of map_sapia function, including downloading GBIF records 
# - Example downloads GBIF records for Acacia cyclops, 
#   and then makes three different maps (all records, presence/absence and abundance). 

# Load a function from my GitHub to download GBIF records from across South Africa 
devtools::source_url("https://raw.githubusercontent.com/guysutton/sapia_type_maps/master/functions/get_clean_obs_function.R")

# This code is not yet a function. I will do this soon.
# It is just an example of how to use the sapa_type_map function
# by downloading GBIF data, as we obviously don't have access to SAPIA data yet. 

# You can try it for any species of interest below, just make sure you change species
# name. 

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

# Import base map
world <- rnaturalearth::ne_countries(scale = "medium",  
                                     returnclass = "sf")

# Keep only South Africa 
world <- world %>%
  dplyr::filter(name == "South Africa")

##############################################################
# - Example #1: Plot distribution of Acacia cyclops in RSA
##############################################################

# ----- Get GBIF records
gps_species <- get_clean_obs(genus = "Acacia", species = "cyclops",
                             # Only pull records for southern Africa 
                             lonlim = c(15.5, 33.5), 
                             latlim = c(-35, -21.75))
gps_species

# ----- Process GBIF records 
gps_species <- gps_species %>%
  dplyr::transmute(plant_species = "Acacia cyclops",
                   longitude = longitude,
                   latitude = latitude) 
gps_species

# ----- Filter only GPS points within South Africa 

# Make RSA shapefile into SpatialPolygonDataFrame
rsa_sp <- as(world, 'Spatial')

# Make GPS points into SpatialPointsDataFrame
gps_sp <- SpatialPointsDataFrame(coords = gps_species[, c("longitude", "latitude")], 
                                 data = gps_species,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Filter and keep only points within SpatialPolygon
gps_filter <- gps_sp[!is.na(over(gps_sp, as(rsa_sp, "SpatialPolygons"))), ]

# Make filtered points into a dataframe
gps_filter <- as.data.frame(gps_filter)

head(gps_filter)

# ----- Run the map_sapia function 

# Make sure the function is loaded into your workspace 
# - Currently, you can load the function from my GitHub account 
devtools::source_url("https://raw.githubusercontent.com/guysutton/sapia_type_maps/master/scripts/working_functions/01_sapia_type_map.R")

# Map type #1: Plot all records
map_sapia(data = gps_filter, 
          # Which map do you want?
          map_type = "all",
          # Which species should we plot? 
          species = "Acacia cyclops")

ggsave("./figures/example_script_01_map_acacia_cyclops_all.png",
       width = 8,
       height = 6,
       dpi = 600)

# Map type #2: Plot presence/absence map
map_sapia(data = gps_filter, 
          # Which map do you want?
          map_type = "presence",
          # Which species should we plot? 
          species = "Acacia cyclops")

ggsave("./figures/example_script_01_map_acacia_cyclops_pres.png",
       width = 8,
       height = 6,
       dpi = 600)

# Map type #3: Plot abundance map
map_sapia(data = gps_filter, 
          # Which map do you want?
          map_type = "abundance",
          # Which species should we plot? 
          species = "Acacia cyclops")

ggsave("./figures/example_script_01_map_acacia_cyclops_abun.png",
       width = 8,
       height = 6,
       dpi = 600)
