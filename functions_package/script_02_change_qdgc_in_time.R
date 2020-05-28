# Script 02 - Calculate change in QDGC occupied over time 

# Load libraries
library(tidyverse)

###
# - Make function to calculate change in QDGC by time period
###

change_in_qdgc <- function(data = sapia_plant_db,
                           year_split,
                           year_stop,
                           col = plant_species,
                           species = all_names,
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
  
  # Break dataset into early time period
  data_early <- data %>% 
    dplyr::filter(year, between(year, 1960, {{ year_split }})) %>%
    group_by(plant_species) %>%
    dplyr::summarise(
      # Count total number of records in database
      no_records_early   = n(), 
      # Count number of regions each species occurs in
      no_provinces_early = n_distinct(region),
      # Count number of quarter degree grid cells occupied
      no_qds_early       = n_distinct(qdgc)) %>%
    dplyr::mutate(time_period_early = "Till {{ year_split }}")
  
  # Break dataset into later time period
  data_later <- data %>% 
    dplyr::filter(year, between(year, 1960, {{ year_stop }})) %>%
    group_by(plant_species) %>%
    dplyr::summarise(
      # Count total number of records in database
      no_records_late   = n(), 
      # Count number of regions each species occurs in
      no_provinces_late = n_distinct(region),
      # Count number of quarter degree grid cells occupied
      no_qds_late       = n_distinct(qdgc)) %>%
    dplyr::mutate(time_period_later = "Till {{ year_stop }} ")
  
  # Combine datasets 
  data_combined <- left_join(data_early, 
                             data_later, 
                             by = "plant_species")
  
  # Process datasets 
  data_analysis <- data_combined %>%
    dplyr::group_by(plant_species) %>%
    dplyr::mutate(new_qds       = no_qds_late - no_qds_early,
                  new_records   = no_records_late - no_records_early,
                  new_provinces = no_provinces_late - no_provinces_early)
  
  # Reproduce data from table 1 in Wilson and Henderson (2017) - Bothalia
  change_in_qdgc_table <- data_analysis %>%
    dplyr::mutate(no_qds_change = (no_qds_early - no_qds_late) * -1) %>%
    dplyr::select(plant_species, 
                  no_qds_early,
                  no_qds_all = no_qds_late,
                  no_qds_change) %>%
    dplyr::mutate(percentage_change_qds_occupied = 
                    round((100 - (no_qds_early / no_qds_all) * 100), 0)) %>%
    # Make table neat
    dplyr::select(plant_species,
                  no_qdgc_till_split = no_qds_early,
                  no_qdgc_till_end = no_qds_all,
                  no_qdgc_change = no_qds_change,
                  percentage_change_qdgc = percentage_change_qds_occupied)

  print(change_in_qdgc_table)
  
}

########################################################################

# Test function - make Table 5 from Wilson and Henderson 2017

table_5_example <- change_in_qdgc(year_split = 2000,
                                  year_stop = 2016, 
    species = c("Acacia longifolia (Andrews) Willd.",
                "Acacia cyclops G.Don", 
                "Acacia saligna (Labill.) Wendl.", 
                "Acacia pycnantha Benth.", 
                "Acacia mearnsii De Wild.", 
                "Acacia baileyana F. Muell.", 
                "Acacia melanoxylon R.Br.", 
                "Acacia podalyriifolia G.Don", 
                "Acacia elata Benth."))
    


change_in_qdgc(year_split = 2000,
               year_stop = 2019)





# Compare distributions pre-2000 versus between 2001 and 2016
# Reproduce data from table 1 in Wilson and Henderson (2017) - Bothalia
table1_data <- species_compare(year_split = 2000, 
                               year_stop  = 2016)


table_1 <- table1_data %>%
  dplyr::mutate(no_qds_change = (no_qds_early - no_qds_late) * -1) %>%
  dplyr::select(plant_species, 
                no_qds_early,
                no_qds_all = no_qds_late,
                no_qds_change) %>%
  dplyr::arrange(desc(no_qds_change))
table_1

# Reproduce data from table 1 in Wilson and Henderson (2017) - Bothalia
table_3 <- table1_data %>%
  dplyr::filter(plant_species %in% c("Acacia longifolia (Andrews) Willd.",
                                     "Acacia cyclops G.Don",
                                     "Acacia saligna (Labill.) Wendl.",
                                     "Acacia pycnantha Benth.",
                                     "Acacia mearnsii De Wild.",
                                     "Acacia baileyana F. Muell.",
                                     "Acacia melanoxylon R.Br.",
                                     "Acacia podalyriifolia G.Don",
                                     "Acacia elata Benth.")) %>%
  dplyr::mutate(no_qds_change = (no_qds_early - no_qds_late) * -1) %>%
  dplyr::select(plant_species, 
                no_qds_early,
                no_qds_all = no_qds_late,
                no_qds_change) %>%
  dplyr::mutate(percentage_change_qds_occupied = 
                  round((100 - (no_qds_early / no_qds_all) * 100), 0))
table_3