###
# - Produce density category maps from SAPIA
###

# SAPIA asks users to classify each record as 
# - rare, present, occassional, frequent, abundant, very abundant

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

# Load raw data
raw_data <- readxl::read_xlsx("./data_raw/Lantana camara SAPIA version 2.xlsx")
head(raw_data)

# Process data 
data <- raw_data %>%
  janitor::clean_names() %>%
  dplyr::rename(longitude = dec_long, 
                latitude  = dec_lat) %>%
  tidyr::drop_na(longitude, latitude)

#############################################################################
#############################################################################
#############################################################################

# Define function
map_density <- function(data, 
                        col, 
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
  
  #####
  # - Section 1: Get and clean data    
  ####
  
  # Process dataset 
  
  # Filter which plant species to plot 
  # Must use %in%, not ==
  data <- dplyr::filter({{ data }}, sapia_tax_id %in% {{ species }}) %>% 
    mutate(abun = dplyr::recode(abun, 
                                Rare = "Rare",
                                Present = "Present",
                                Occas = "Occassional",
                                Freq = "Frequent",
                                Abun = "Abundant",
                                # V Abun is not picked up normally, hence backticks 
                                `V Abun` = "Very abundant"))
  ####
  # - Section 2: Filter only GPS points within South Africa 
  ####
  
  # Make RSA shapefile into SpatialPolygonDataFrame
  rsa_sp <- as(world, 'Spatial')
  
  # Make GPS points into SpatialPointsDataFrame
  gps_sp <- SpatialPointsDataFrame(coords = data[, c("longitude", "latitude")], 
                                   data = data,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  # Filter and keep only points within SpatialPolygon
  filter_data <- gps_sp[!is.na(over(gps_sp, as(rsa_sp, "SpatialPolygons"))), ]
  
  # Make filtered points into a dataframe
  filter_data <- as.data.frame(filter_data) %>%
    dplyr::select(longitude, latitude, abun) %>%
    tidyr::drop_na() %>%
    mutate(abun = as.factor(abun)) %>%
    mutate(abun = fct_relevel(abun,
                              "Rare",
                              "Present",
                              "Occassional",
                              "Frequent",
                              "Abundant",
                              "Very abundant"))
  
  # Set ggplot theme
  theme_set(theme_classic() +
              theme(panel.border = element_rect(colour = "black", fill = NA),
                    axis.text = element_text(colour = "black"),
                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                    axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                    legend.position = "none"))
  
  
  #####
  # - Section 3: Plot map
  #####
  
  # Plot map
  
  p <- ggplot(data = world) +
    scale_fill_viridis_c(na.value = "white") +
    geom_sf(fill = "NA") +
    geom_point(data = filter_data, aes(x = longitude,
                                       y = latitude)) +
    labs(x = "Longitude", 
         y = "Latitude") + 
    facet_wrap(~abun, ncol = 3) +
    coord_sf(xlim = c(15.5, 33.5), 
             ylim = c(-35, -21.75), 
             expand = FALSE) +
    annotation_scale(location = "br", # 
                     style = "ticks", 
                     width_hint = 0.150) +
    annotation_north_arrow(location = "br", 
                           which_north = "true", 
                           pad_x = unit(-0.025, "in"), 
                           pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) 
  
  # Print map
  
  suppressWarnings(warning("Scale on map varies by more than 10%, scale bar may be inaccurate"))
  
  p
  
  
  
}

##################################################################
##################################################################
##################################################################

# Take function for test run
map_density(data = data,
            col = sapia_tax_id,
            species = "Lantana camara L. sensu lato")

# Save map
ggsave("./figures/map_sapia_density_lantana.png",
       dpi = 600, 
       width = 10,
       height = 12)
