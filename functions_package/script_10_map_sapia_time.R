###
# - Produce decadal maps from SAPIA
###

# This function plots the cumulative GPS records per decade 
# ranging from 1980 up to 2020 (4 decades)

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
  tidyr::drop_na(longitude, latitude) %>%
  # Add a year column
  mutate(year = as.numeric(stringr::str_extract(data$date, "[^-]+")))


#################################################################################
#################################################################################
#################################################################################

map_time <- function(data,
                     time_frame = "10",
                     col = sapia_tax_id,
                     species,
                     ...) {
  
  #####
  # - Section 0: Download map for South Africa   
  ####
  
  # Import base map
  world <- rnaturalearth::ne_countries(scale = "medium",  
                                       returnclass = "sf") %>%
    # Keep only South Africa 
    dplyr::filter(name == "South Africa")
  
  ####
  # - Section 1: Filter only GPS points within South Africa 
  ####
  
  # Which species do we want to plot? 
  data <- filter({{ data }}, {{ col }} %in% {{ species }})
  
  # Make RSA shapefile into SpatialPolygonDataFrame
  rsa_sp <- as(world, 'Spatial')
  
  # Make GPS points into SpatialPointsDataFrame
  gps_sp <- SpatialPointsDataFrame(coords = data[, c("longitude", "latitude")], 
                                   data = data,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # Filter and keep only points within SpatialPolygon
  filter_data <- gps_sp[!is.na(over(gps_sp, as(rsa_sp, "SpatialPolygons"))), ] 
  filter_data <- filter_data %>%
    as.data.frame(filter_data) %>%
    dplyr::select(longitude, latitude, time_period, year) %>%
    tidyr::drop_na() 
  
  #####
  # - Section 2: Split data into respective time periods
  ####
  
  # 1. Split data by decade, from 1980 till 2019
  #    - Cumulative records in all decades leading up to that point
  
  data_80 <- filter_data %>%
    dplyr::filter(year, between(year, 1980, 1989)) %>%
    dplyr::mutate(time_period = "1980-1989")
  data_90 <- filter_data %>%
    dplyr::filter(year, between(year, 1980, 1999)) %>%
    dplyr::mutate(time_period = "1980-1999")
  data_00 <- filter_data %>%
    dplyr::filter(year, between(year, 1980, 2009)) %>%
    dplyr::mutate(time_period = "1980-2009")
  data_10 <- filter_data %>%
    dplyr::filter(year, between(year, 1980, 2019)) %>%
    dplyr::mutate(time_period = "1980-2019")
  
  # Combine decadal data into one dataframe
  data_decade <- bind_rows(data_80, data_90, data_00, data_10)
  
  # 2. Split data into 5-year periods, from 2000 till 2019
  #    - Cumulative records in all 5-year periods leading up to that point
  
  data_a <- filter_data %>%
    dplyr::filter(year, between(year, 2000, 2004)) %>%
    dplyr::mutate(time_period = "2000-2004")
  data_b <- filter_data %>%
    dplyr::filter(year, between(year, 2000, 2009)) %>%
    dplyr::mutate(time_period = "2000-2009")
  data_c <- filter_data %>%
    dplyr::filter(year, between(year, 2000, 2014)) %>%
    dplyr::mutate(time_period = "2000-2014")
  data_d <- filter_data %>%
    dplyr::filter(year, between(year, 2000, 2019)) %>%
    dplyr::mutate(time_period = "2000-2019")
  
  # Combine 5-year data into one dataframe
  data_5yr <- bind_rows(data_a, data_b, data_c, data_d)
  
  # Set ggplot theme
  theme_set(theme_classic() +
              theme(panel.border = element_rect(colour = "black", fill = NA),
                    axis.text = element_text(colour = "black"),
                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                    axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                    legend.position = "none"))
  
  #####
  # -  Section 3: Plot maps 
  #####
  
  # If users specify time_frame = "10",
  # the following map is returned showing cumulative records by decade.  
  
  if (time_frame == "10") {
    
    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_point(data = data_decade, aes(x = longitude,
                                         y = latitude,
                                         group = time_period)) +
      labs(x = "Longitude", 
           y = "Latitude") + 
      facet_wrap(~time_period, ncol = 2) +
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
    
  }
  
  # If users specify time_frame = "10",
  # the following map is returned showing cumulative records by decade.  
  
  if (time_frame == "5") {
    
    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_point(data = data_5yr, aes(x = longitude,
                                      y = latitude,
                                      group = time_period)) +
      labs(x = "Longitude", 
           y = "Latitude") + 
      facet_wrap(~time_period, ncol = 2) +
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
    
  }
  
  # Print the map to console
  p
  
}

#############################################################################
#############################################################################
#############################################################################

# Take function for a test run 
# By default, decadal map is produced 
map_time(data = data,
         species = "Lantana camara L. sensu lato")

# Manually ask for decadal map
map_time(data = data,
         time_frame = 10,
         species = "Lantana camara L. sensu lato")

# Manually ask for 5-yr map
map_time(data = data,
         time_frame = 5,
         species = "Lantana camara L. sensu lato")