# Define find_agent function to find correct biocontrol agent names in SAPIA database

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

#############################################################################
#############################################################################
#############################################################################

# Define function
find_agent <- function(data = sapia_plant_db,
                       col = agent_name,
                       search_name,
                       ...) { 
  
  #####
  # - Section 1: Process data     
  ####
  
  # Pull a vector with all the plant species names 
  agent_names <- {{ data }} %>%
    dplyr::filter(agent_name %in% 
                    stringr::str_subset(agent_name, {{ search_name }})) %>%
    dplyr::distinct(agent_name) %>%
    dplyr::pull()
  
  # Print table 
  
  agent_names
  
}

##############################################################################
##############################################################################
##############################################################################

# Test function
find_agent(search_name = "Octotoma")

# Find plant and agent name, and then map 
plants_to_map <- find_plant(search_name = "Lantana")
agents_to_map <- find_agent(search_name = "Hypena")
map_biocontrol(species_plant = plants_to_map, 
               species_agent = agents_to_map)

# Find plant and agent name, and then map 
plants_to_map <- find_plant(search_name = "Lantana")
map_biocontrol_many(species_plant = plants_to_map)