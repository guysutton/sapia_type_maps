# Load and process raw SAPIA database

# Load libraries
library(tidyverse)

# Import database 
#raw_data <- readxl::read_xlsx("./data_raw/tAP_Main.xlsx")
raw_data <- readr::read_csv2("./data_raw/tAP_Main.csv",
                             col_types = cols(.default = "c",
                                              "Record_number" = "n",
                                              "Agent Name" = "c",
                                              "Dec Lat" = "n",
                                              "Dec Long" = "n"))
                        
# Clean dataset 
sapia_plant_db <- raw_data %>%
  # Clean column names
  janitor::clean_names() %>%
  # Add year column
  mutate(year = as.numeric(stringr::str_extract(date, "[^-]+"))) %>%
  # Select only columns of interest, and rename some columns 
  dplyr::select(plant_species = sapia_tax_id,
                record_number,
                year,
                country, 
                region,
                qdgc = x1_4_deg_sq,
                longitude = dec_long,
                latitude = dec_lat,
                locality = locality_route,
                density = abun,
                forest:deepsand,
                agent_name,
                agent_release, 
                agent_abundance = abundance,
                host_damage) %>%
  # Sort alphabetically and by year 
  dplyr::arrange(plant_species, year) %>%
  # Remove Invader Absent 
  dplyr::filter(plant_species != "Invader Absent") %>%
  # Extract degrees for long/lat from QDGC
  dplyr::mutate(lat_cell = (as.numeric(stringr::str_sub(qdgc, 
                                            start = 1, 
                                            end = 2)) * -1),
                lon_cell = (as.numeric(stringr::str_sub(qdgc, 
                                            start = 3, 
                                            end = 4))),
                big_square = stringr::str_sub(qdgc,
                                              start = 5,
                                              end = 5),
                small_square = stringr::str_sub(qdgc,
                                                start = 6,
                                                end = 6)) %>%
  # Calculate midpoints of latitude QDGC
  dplyr::mutate(
    lat_mp = case_when(
    big_square %in% c("A", "B") ~ as.numeric(lat_cell - 0.000),
    big_square %in% c("C", "D") ~ as.numeric(lat_cell - 0.300)),
    lat_mp = case_when(
      small_square %in% c("A", "B") ~ as.numeric(lat_mp - 0.075),
      small_square %in% c("C", "D") ~ as.numeric(lat_mp - 0.225))) %>%
  # Calculate midpoints of longitude QDGC ( in degree minutes)
  dplyr::mutate(
    lon_mp = case_when(
      big_square %in% c("A", "C") ~ as.numeric(lon_cell + 0.000),
      big_square %in% c("B", "D") ~ as.numeric(lon_cell + 0.300)),
    lon_mp = case_when(
      small_square %in% c("A", "C") ~ as.numeric(lon_mp + 0.075),
      small_square %in% c("B", "D") ~ as.numeric(lon_mp + 0.225))) %>%
  # Extract lat and lon minutes to convert to decimal degrees 
  dplyr::mutate(lat_mins = as.numeric(str_sub(lat_mp, start = -3)) / 10,
                lon_mins = as.numeric(str_sub(lon_mp, start = -3)) / 10) %>%
  # Convert lat and lon minutes to decimal degrees 
  dplyr::mutate(lat_dec = lat_mins / 60,
                lon_dec = lon_mins / 60) %>%
  # Extract lat and lon degrees 
  dplyr::mutate(lat_deg = as.numeric(str_sub(lat_mp, 
                                             start = 1,
                                             end = 3)),
                lon_deg = as.numeric(str_sub(lon_mp, 
                                              start = 1,
                                              end = 2))) %>%
  # Calculate final latitude and longitude for QDGC's (decimal degrees)
  dplyr::mutate(lat_qdgc = lat_deg - lat_dec,
                lon_qdgc = lon_deg + lon_dec) %>%
  # Drop columns with qdgc calculations
  dplyr::select(-(lat_cell:lon_deg)) %>%
  # Combine lat/long mid-points with actual GPS co-ords 
  # If a record has actual GPS coords, then we drop the QDGC coords
  # If no coords, then impute QDGC coords. 
  dplyr::mutate(
    latitude = case_when(
      !is.na(latitude) ~ as.numeric(latitude),
      is.na(latitude) ~ as.numeric(lat_qdgc)),
    longitude = case_when(
      !is.na(longitude) ~ as.numeric(longitude),
      is.na(longitude) ~ as.numeric(lon_qdgc))) %>%
  # Drop columns with qdgc calculations
  dplyr::select(-(lat_qdgc:lon_qdgc)) %>%
  # Recode some levels within factors that are repeated 
  dplyr::mutate(agent_abundance = forcats::fct_recode(agent_abundance, 
                                                      "Abundant" = "abundant",
                                                      "Present" = "present")) %>%
  # Remove the rows that have no QDGC or coords (very few)
  tidyr::drop_na(qdgc)

# Save processed data to PC
write_excel_csv2(sapia_plant_db,
                 "./data_proc/sapia_db_clean.csv")

##############################################################################
##############################################################################
##############################################################################


