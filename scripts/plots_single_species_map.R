# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

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

##########################
### Function to plot map
########################## 

# Write function to plot individual map for different plant species
# Ultimately, the function should look something like:

# plot_map(plant_species = "name_of_plant_to_map") 
#  - Take "name_of_plant_to_map" from plant_species column
#  - Not sure if my issue is having plant_species as a column in dataframe, 
#    and an argument in the function??? 

# Attempt at function 
plot_map <- function(data = raw_data, species_name){
  
  # Filter which plant species to plot 
  df = subset(raw_data, plant_species == species_name)
  
  # Plot map
  ggplot(data = world) +
    geom_sf() +
    geom_point(data = df, aes(x = longitude, 
                              y = latitude)) + 
    labs(x = "Longitude", 
         y = "Latitude") + 
    coord_sf(xlim = c(10, 38), 
             ylim = c(-35, -16.5), 
             expand = FALSE)
}

# Attempt to use function 
plot_map(species_name = c("Sporobolus pyramidalis"))
plot_map(species_name = c("Eragrostis curvula"))

         