# --- Source functions required 
if(!exists("get_clean_obs_function", mode="function")) 
  source("C:/Users/g12s0/OneDrive/Statistics/utility_functions/get_clean_obs_function.r")


##################################################################
# - Example #1: Plot distribution of Sporobolus pyramidalis in RSA
##################################################################

# ----- Get GBIF records
gps_species <- get_clean_obs(genus = "Sporobolus", species = "pyramidalis",
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
