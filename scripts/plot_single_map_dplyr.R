# Libraries
library(rlang)

# Attempt at function 
plot_map <- function(data, 
                     plant_species,
                     ...){
  
  # Filter which plant species to plot 
  # Must use %in%, not ==
  df <- filter(raw_data, plant_species %in% !!plant_species)
  
  # Plot map
  ggplot(data = world) +
    geom_sf(fill = "NA") +
    geom_point(data = df, aes(x = longitude, 
                              y = latitude)) + 
    labs(x = "Longitude", 
         y = "Latitude") + 
    coord_sf(xlim = c(15.5, 33.5), 
             ylim = c(-35, -21.75), 
             expand = FALSE) 
}

# Attempt to use function 
plot_map(data = raw_data, 
         plant_species = c("Sporobolus pyramidalis")) 



plot_map(plant_species = c("Eragrostis curvula"))
plot_map(plant_species = c("Sporobolus pyramidalis",
                           "Eragrostis curvula"))
