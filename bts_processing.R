# Libraries -------------------------------------------------------------------
library(tidyverse)

# Read in air traffic data -----------------------------------------------------
dir <- "data\\bts_data"

files <- list.files(path = dir, pattern = "T_T100", full.names = TRUE)

airtraffic_list <- lapply(files, function(elem) {
  
  df <- read_csv(elem)
  return(df)
  
})

airtraffic <- bind_rows(airtraffic_list) %>% janitor::clean_names()


# Add in codes used in several columns ----------------------------------------

data_source <- read_csv(paste0(dir, "\\DATA_SOURCE.csv")) %>% janitor::clean_names()
service_class <- read_csv(paste0(dir, "\\SERVICE_CLASS.csv")) %>% janitor::clean_names()
aircraft_type <- read_csv(paste0(dir, "\\AIRCRAFT_TYPE.csv")) %>% janitor::clean_names()
aircraft_group <- read_csv(paste0(dir, "\\AIRCRAFT_GROUP.csv")) %>% janitor::clean_names()


airtraffic <- airtraffic %>% 
  
  # add in data_source code descriptions
  left_join(., data_source, by=join_by(data_source == code)) %>%
  rename(data_source_description = description) %>%
  
  # add in service_class code descriptions
  left_join(., service_class, by = join_by(class == code)) %>%
  rename(service_class_description = description) %>%
  
  # add in aircraft_type code descriptions
  left_join(., aircraft_type, by = join_by(aircraft_type == code)) %>%
  rename(aircraft_type_description = description) %>%
  
  # add in aircraft_group code descriptions
  left_join(., aircraft_group, by = join_by(aircraft_group == code)) %>%
  rename(aircraft_group_description = description)


# Narrow down major/familiar air carriers ------------------------------------

air_carriers <- c("Southwest Airlines Co.", "Delta Air Lines Inc.",
                  "United Air Lines Inc.", "American Airlines Inc.", 
                  "SkyWest Airlines Inc.", "Allegiant Air", "Frontier Airlines Inc.",
                  "JetBlue Airways", "Spirit Air Lines", "Continental Air Lines Inc.")


airtraffic <- airtraffic %>%
  filter(unique_carrier_name %in% air_carriers)

# Standardize air carrier names --------------------------------------------

airtraffic <- airtraffic %>%
  mutate(unique_carrier_name = case_when(
    
    str_detect(unique_carrier_name, "Southwest") ~ "southwest airlines",
    str_detect(unique_carrier_name, "Delta") ~ "delta airlines",
    str_detect(unique_carrier_name, "United") ~ "united airlines",
    str_detect(unique_carrier_name, "American") ~ "american airlines",
    str_detect(unique_carrier_name, "SkyWest") ~ "skywest airlines",
    str_detect(unique_carrier_name, "Allegiant") ~ "allegiant airlines",
    str_detect(unique_carrier_name, "Frontier") ~ "frontier airlines",
    str_detect(unique_carrier_name, "JetBlue") ~ "jetblue airways",
    str_detect(unique_carrier_name, "Spirit") ~ "spirit airlines",
    str_detect(unique_carrier_name, "Continental") ~ "continental airlines",
    TRUE ~ NA
    
  ))

# Summary statistics -------------------------------------------------------

sum_stats <- airtraffic %>%
  group_by(unique_carrier_name) %>%
  summarise(
    count = n()
)

# Output -------------------------------------------------------------------

save(airtraffic, file = "airtraffic.Rdata")

