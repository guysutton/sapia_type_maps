# Define map_biocontrol function to plot biocontrol agent distributions
# over the distribution of its host plant 

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

######################################################################
######################################################################
######################################################################

# Define function
map_biocontrol <- function(data = sapia_plant_db, 
                           col = plant_species, 
                           species_plant,
                           species_agent,
                           add_copyright = FALSE, 
                           add_name = TRUE, 
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
  
  #####
  # - Section 1: Process plant data to plot    
  ####
  
  # Filter which plant species to plot 
  data <- filter({{ data }}, {{ col }} %in% {{ species_plant }})
  
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
  
  # Set ggplot theme
  theme_set(theme_classic() +
              theme(panel.border = element_rect(colour = "black", fill = NA),
                    axis.text = element_text(colour = "black"),
                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                    axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                    legend.position = "none"))
  
  #####
  # - Section 2: Process agent data to plot    
  ####
  
  # Filter which agent to plot
  agent_data <- filter_data %>%
    dplyr::filter(agent_name %in% {{ species_agent }}) %>%
    tidyr::drop_na(agent_name)
  
  #####
  # - Section 3: Plot biocontrol agent distribution over invader distribution   
  ####
  
  p <- ggplot(data = world) +
    scale_fill_viridis_c(na.value = "white") +
    geom_sf(fill = "NA") +
    # Plot plant data 
    geom_point(data = filter_data, aes(x = longitude, 
                                       y = latitude),
               colour = "gray75") +
    # Plot bicontrol agent data 
    geom_point(data = agent_data, aes(x = longitude, 
                                      y = latitude),
               colour = "black") +
    labs(x = "Longitude", 
         y = "Latitude") + 
    coord_sf(xlim = c(15.5, 33.5), 
             ylim = c(-35, -21.75), 
             expand = FALSE) +
    annotation_scale(location = "br", # 
                     style = "ticks", 
                     width_hint = 0.150) +
    annotation_north_arrow(location = "br", 
                           which_north = "true", 
                           pad_x = unit(0.175, "in"), 
                           pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotate("text", 
             x = 16.5, 
             y = -22.5, 
             fontface = "italic",
             size = 5,
             hjust = 0,
             label = {{ species_agent }})
  
  # Print map
  
  p
  
}

########################################################################
########################################################################
########################################################################

# Take function for test run 
map_biocontrol(species_plant = "Lantana camara L. sensu lato",
               species_agent = "Octotoma scabripennis")

# Save to disc
ggsave("./figures/map_biocontrol_octotoma_scabripennis.png",
       dpi = 600, 
       height = 6,
       width = 8)

# Take function for test run 
map_biocontrol(species_plant = "Lantana camara L. sensu lato",
               species_agent = "Falconia intermedia")

# Save to disc
ggsave("./figures/map_biocontrol_falconia_intermedia.png",
       dpi = 600, 
       height = 6,
       width = 8)