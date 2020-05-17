# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)

# Import raw coords
raw_data <- readxl::read_xlsx("./data_raw/species_gps.xlsx")

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

# Attempt at function 
plot_map <- function(data, 
                     plant_species,
                     ...){
  
  # Filter which plant species to plot 
  # Must use %in%, not ==
  df <- filter({{ raw_data }}, plant_species %in% {{ plant_species }})
  
  # Plot map
  ggplot(data = world) +
    geom_sf(fill = "NA") +
    geom_point(data = df, aes(x = longitude, 
                              y = latitude)) + 
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
             x = 18, 
             y = -22.5, 
             fontface = "italic",
             label = {{ plant_species }})
}

# Attempt to use function 
plot_map(data = raw_data, 
         plant_species = c("Sporobolus pyramidalis"))
plot_map(data = raw_data, 
         plant_species = c("Eragrostis curvula"))

# Save maps 
ggsave("./figures/sporobolus_pyramidalis_map.png",
       width = 8,
       height = 6)
