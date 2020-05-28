# Define plant_summary_records function to extract table of total records,
# number of regions occupied and no quarter degree grid cells occupied
# by each species. 

# Load packages
library(tidyverse)

#############################################################################
#############################################################################
#############################################################################

# Define function
plant_summary_records <- function(data = sapia_plant_db, 
                                  species = all_names, 
                                  col = plant_species,  
                                  ...) {
  
  #####
  # - Section 1: Process data to calculate summary statistics    
  ####
  
  # Pull a vector with all the plant species names 
  all_names <- {{ data }} %>%
    dplyr::distinct(plant_species) %>%
    dplyr::pull()
  
  # By default, we will calculate summary statistics on all species
  # But, we can specify a subset of species, using the 'species' argument, if necessary. 
  
  # Subset data, if necessary. 
  data <- filter({{ data }}, {{ col }} %in% {{ species }})
  
  #####
  # - Section 2: Calculate summary statistics   
  ####
  
  plant_summary <- data %>%
    # Produce a summary by species 
    dplyr::group_by(plant_species) %>%
    dplyr::summarise(
      # Count total number of records in database
      no_records    = n(), 
      # Count number of regions each species occurs in
      no_regions    = n_distinct(region),
      # Count number of quarter degree grid cells occupied
      no_qdgc       = n_distinct(qdgc)) %>%
    dplyr::arrange(desc(no_qdgc))
  
  # Print table
  
  plant_summary

}

################################################################################
################################################################################
################################################################################