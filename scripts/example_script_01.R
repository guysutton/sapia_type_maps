# --- Source functions required 
if(!exists("get_clean_obs_function", mode="function")) 
  source("./functions/get_clean_obs_function.R")

devtools::source_url("https://raw.githubusercontent.com/guysutton/sapia_type_maps/master/functions/get_clean_obs_function.R")

##################################################################
# - Example #1: Plot distribution of Sporobolus pyramidalis in RSA
##################################################################

# This code is not yet a function. I will do this soon.
# It is just an example of how to use the sapa_type_map function
# by downloading GBIF data, as we obviously don't have access to SAPIA data yet. 
# Would be helluva cool though to have the SAPIA database pre-loaded into an 
# R package with this functionality. 

# You can try it for any species of interest below, just make sure you change species
# name in line 20, 21, 30, and 62. (repetition be required when I function this!!!)

# ----- Get GBIF records
gps_species <- get_clean_obs(genus   = "Sporobolus", 
                             species = "pyramidalis",
                             # Only pull records for southern Africa
                             lonlim = c(15.5, 33.5), 
                             latlim = c(-35, -21.75))
gps_species

# ----- Process GBIF records 
gps_species <- gps_species %>%
  dplyr::transmute(plant_species = "Sporobolus pyramidalis",
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

# Make sure the function is loaded into your workspace (i.e. run that script first)

map_sapia(data = gps_filter, 
          # Plot abundance map, LOGICAL
          plot_abun = TRUE,
          # Which species should we plot? 
          plant_species = "Sporobolus pyramidalis")

# You save (figure it optimised to be viewed as saved .png, not in R console)
ggsave("example_script_01_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# Guy save
ggsave("./figures/example_script_01_map.png",
       width = 8,
       height = 6,
       dpi = 600)

##############################################################
# - Example #2: Plot distribution of Acacia cyclops in RSA
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

# Make sure the function is loaded into your workspace (i.e. run that script first)

map_sapia(data = gps_filter, 
          # Plot abundance map, LOGICAL
          plot_abun = TRUE,
          # Which species should we plot? 
          plant_species = "Acacia cyclops")

ggsave("./figures/example_script_01_map_acacia_cyclops.png",
       width = 8,
       height = 6,
       dpi = 600)

map_sapia(data = gps_filter, 
          # Plot abundance map, LOGICAL
          plot_abun = FALSE,
          # Which species should we plot? 
          plant_species = "Acacia cyclops")
ggsave("./figures/example_script_01_map_acacia_cyclops_pres.png",
       width = 8,
       height = 6,
       dpi = 600)
