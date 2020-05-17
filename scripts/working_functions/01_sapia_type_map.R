# Define function to make a SAPIA-style map
# - Users can choose to plot presence/absense map
#   or an abundance map. 

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

# Import raw coords
#raw_data <- readxl::read_xlsx("./data_raw/species_gps.xlsx")
raw_data <- readr::read_csv("https://github.com/guysutton/sapia_type_maps/blob/master/data_raw/species_gps.csv")

# Process co-ords 
raw_data <- raw_data %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))
head(raw_data)

# Set ggplot theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

# Import base map
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")

# Keep only South Africa 
world <- world %>%
  dplyr::filter(name == "South Africa")

#############################################################################
#############################################################################
#############################################################################

# Define function
map_sapia <- function(data, 
                      plant_species,
                      plot_abun = FALSE,
                      ...) {
  
  #####
  # - Section 1: Set up raster cells to derive abundances   
  ####
  
  # Initialise a raster of uniform cells over South Africa 
  rast <- raster(xmn= -180, 
                 ymn= -90, 
                 xmx = 180, 
                 ymx = 90, 
                 resolution = 0.5,
                 crs = '+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
  
  # Set the resolution of the raster 
  x_res <- 0.25
  y_res <- 0.25 
  res(rast) <- c(x_res, y_res)  
  res(rast)
  
  # Filter which plant species to plot 
  # Must use %in%, not ==
  data <- filter({{ data }}, plant_species %in% {{ plant_species }})
  
  # Get xy-cords into a matrix 
  xy <- data %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()
  
  # Count the number of GPS points in each raster cell
  r0 <- rasterize(xy, 
                  rast, 
                  fun = "count")
  
  #####
  # - Section 2: Filter data for presence/absense map   
  ####
  
  # Convert matrix to data frame
  datf <- as.data.frame(r0, 
                        xy=TRUE)
  
  
  # Filter out all cells with 0 records
  data_pres <- datf %>%
    dplyr::filter(layer > 0)
  
  #####
  # - Section 3: Filter data for abundance map   
  ####
  
  # Replace NA with 0
  data_abun <- datf %>%
    tidyr::drop_na(layer)
  
  # Cut abundance vector into abundance categories
  # Cat1 = 0 records (filtered out = white cells)
  # Cat2 = 1 record
  # Cat3 = 2-5 records
  # Cat4 = 6-25 records
  # Cat5 = 25+
  data_abun$layer <- 
    cut(data_abun$layer, 
        breaks = c(0, 1, 2, 5, 25), 
        labels = c("1", "2-5", "6-25", "25+"))
  
  #####
  # -  Section 4: Plot presence/absense map
  #####
  
  # If users specify plot_abun = FALSE,
  # the following map is returned. 
  
  if (plot_abun == FALSE) {
    
    # Plot map
    
    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_point(data = data_pres, aes(x = x,
                                       y = y,
                                       fill = layer)) +
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
               label = {{ plant_species }})
    
  }
  
  #####
  # -  Section 5: Plot abundance map
  #####
  
  # If users specify plot_abun = TRUE,
  # the following map is returned. 
  
  if (plot_abun == TRUE) {
    
    # Plot abundance map
    p <- ggplot(data = world) +
      geom_sf(fill = "white") +
      geom_tile(data = data_abun, aes(x = x,
                                      y = y,
                                      fill = layer)) +
      scale_fill_manual(values = c("gray75", 
                                   "gray50", 
                                   "gray25", 
                                   "black"),
                        drop = FALSE,
                        labels = c("1", "2-5", "6-25", "25+")) +
      labs(x = "Longitude", 
           y = "Latitude",
           fill = "No. of records") + 
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
      theme(legend.position = c(0.14, 0.775),
            legend.title = element_text(size = 12.5),
            legend.text = element_text(size = 12.5)) +
      annotate("text", 
               x = 16.5, 
               y = -22.5, 
               fontface = "italic",
               size = 5,
               hjust = 0,
               label = {{ plant_species }})
  }
  
  # Print the plot
  p
  
}

#############################################################################
#############################################################################
#############################################################################

# Take the function for a test drive

# Plot abundance map for Eragrostis curvula 
map_sapia(data = raw_data, 
          # Plot abundance map, LOGICAL
          plot_abun = TRUE,
          # Which species should we plot? 
          plant_species = "Eragrostis curvula")
ggsave("./figures/era_curv_abun_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# Plot abundance map for Sporobolus pyramidalis 
map_sapia(data = raw_data, 
          # Plot abundance map, LOGICAL
          plot_abun = TRUE,
          # Which species should we plot? 
          plant_species = "Sporobolus pyramidalis")
ggsave("./figures/spo_pyr_abun_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# Plot presence/absence map for Eragrostis curvula 
map_sapia(data = raw_data, 
          # Plot abundance map, LOGICAL
          plot_abun = FALSE,
          # Which species should we plot? 
          plant_species = "Eragrostis curvula")
ggsave("./figures/era_curv_pres_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# Plot presence/absence map for Sporobolus pyramidalis 
map_sapia(data = raw_data, 
          # Plot abundance map, LOGICAL
          plot_abun = FALSE,
          # Which species should we plot? 
          plant_species = "Sporobolus pyramidalis")
ggsave("./figures/spo_pyr_pres_map.png",
       width = 8,
       height = 6,
       dpi = 600)

