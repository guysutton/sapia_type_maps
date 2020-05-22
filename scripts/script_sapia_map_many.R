# Make SAPIA-style map for multiple species (e.g. several species within genus)

####
#### - Download an example dataset 
####

# Download GBIF occurences 

# Load required libraries
library(tidyverse)
library(spocc)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2) 
library(scrubr)
library(mapr)
library(dplyr)
library(tidyr)
library(stringr) 

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

# Extract all Opuntia spp. records from the databases GBIF 
# - Limit to only South African records
gbifopts <- list(country = "ZA") 

# - Now extract records
df <- occ(query = 'Opuntia', 
          from = c('gbif'),
          gbifopts = gbifopts,
          limit=10000)
df

# - Process data 
df_comb <- occ2df(df)
df_comb
df_comb<- dframe(df_comb) %>%
  coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely()
df_comb

# Remove all the authorities 
# - Extract only the first two words 
df_comb <- df_comb %>%
  mutate(name = stringr::word(name, 1, 2))
df_comb

# Which species do we have in our dataset? 
df_comb %>% count(name)

# Keep just the a few example species 
df_comb <- df_comb %>%
  dplyr::filter(name %in% c("Opuntia stricta",
                            "Opuntia salmiana",
                            "Opuntia vulgaris")) %>%
  dplyr::mutate(plant_species = name)

head(df_comb)

#############################################################################
#############################################################################
#############################################################################

# Define function
map_sapia_many <- function(data, 
                           species, 
                           ...) {
  
  #####
  # - Section 0: Download base layer for map   
  ####
  
  # Import base map
  world <- rnaturalearth::ne_countries(scale = "medium",  
                                       returnclass = "sf")
  
  # Keep only South Africa 
  world <- world %>%
    dplyr::filter(name == "South Africa")
  
  # Define number of distinct species
  no_species <- df_comb %>%
    distinct(plant_species) %>%
    nrow(.)
  no_species
  
    # Filter which plant species to plot 
  # Must use %in%, not ==
  data <- filter({{ data }}, plant_species %in% {{ species }})
  
  # ----- Filter only GPS points within South Africa 
  
  # Make RSA shapefile into SpatialPolygonDataFrame
  rsa_sp <- as(world, 'Spatial')
  
  # Make GPS points into SpatialPointsDataFrame
  gps_sp <- SpatialPointsDataFrame(coords = data[, c("longitude", "latitude")], 
                                   data = data,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  # Filter and keep only points within SpatialPolygon
  filter_data <- gps_sp[!is.na(over(gps_sp, as(rsa_sp, "SpatialPolygons"))), ]
  
  # Make filtered points into a dataframe
  filter_data <- as.data.frame(filter_data)
  
  #####
  # -  Section 1: Plot all records across species
  #####
  
    # Plot map
    
    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_jitter(data = filter_data, aes(x = longitude, 
                                          y = latitude,
                                          shape = plant_species),
                  height = 0.1,
                  width = 0.1) +
      scale_shape_manual(values = seq(1:no_species)) +
      labs(x = "Longitude", 
           y = "Latitude",
           shape = "Plant species") + 
      coord_sf(xlim = c(15.5, 33.5), 
               ylim = c(-35, -21.75), 
               expand = FALSE) +
      annotation_scale(location = "br", # 
                       style = "ticks", 
                       width_hint = 0.150) +
      annotation_north_arrow(location = "br", 
                             which_north = "true", 
                             pad_x = unit(0.1, "in"), 
                             pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering) +
      theme(legend.position = "right",
            legend.title = element_text(size = 12.5, face = "bold"),
            legend.text = element_text(size = 12.5, face = "italic"))
  
  # Print the plot
  
  p
  
}


# Test run
map_sapia_many(data = df_comb,
               species = c("Opuntia stricta",
                           "Opuntia salmiana",
                           "Opuntia vulgaris")) 

# Save test run 
ggsave("./figures/map_sapia_many_all.png",
       width = 8,
       height = 6,
       dpi = 600)



