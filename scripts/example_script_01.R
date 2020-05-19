# Example functionality of map_sapia function, including downloading GBIF records 

# Load a function from my GutHub
devtools::source_url("https://raw.githubusercontent.com/guysutton/sapia_type_maps/master/functions/get_clean_obs_function.R")

# This code is not yet a function. I will do this soon.
# It is just an example of how to use the sapa_type_map function
# by downloading GBIF data, as we obviously don't have access to SAPIA data yet. 
# Would be helluva cool though to have the SAPIA database pre-loaded into an 
# R package with this functionality. 

# You can try it for any species of interest below, just make sure you change species
# name. 

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
