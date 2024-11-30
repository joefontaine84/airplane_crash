library(tidyverse)
library(readxl)
library(janitor)

# NTSB incident data ------------------------------------------------------

## Get full file paths for data download ---------------------------------
files <- list.files(path = paste0(getwd(), "/data/ntsb_data"), full.names = TRUE)

ntsb_data <- lapply(files, function(file) {
  
  df <- read_csv(file)
  df <- df %>% 
    janitor::clean_names()
  return(df)
  
}) 

## Apply the names of the file to the ntsb_data object to more easily identify different dataframes -----

file_names <- list.files(path = paste0(getwd(), "/data/ntsb_data"), full.names = FALSE)
file_names <- gsub(".txt", "", file_names)
names(ntsb_data) <- file_names

## Select columns from the "events" table -------------------------------------
events <- ntsb_data$events %>%
  select(ev_id, ntsb_no, ev_type, ev_date, ev_city, ev_state, ev_country,
         ev_highest_injury, mid_air, on_ground_collision, latitude, longitude)


## Select columns from the "aircraft" table ----------------------------------

aircraft <- ntsb_data$aircraft %>%
  select(ev_id, aircraft_key, ntsb_no, acft_make, 
         acft_model, acft_category, oper_pax_cargo, 
         type_fly, far_part, pax_seats, owner_acft)

aircraft <- aircraft %>%
  
  # make the owner_acft column values lowercase for easier processing
  mutate(owner_acft = str_to_lower(owner_acft)) %>%
  
  filter(str_detect(owner_acft, "(southwest|delta|united|american|skywest|
  allegiant|frontier|spirit|continental) air\\s?lines|\\Aunited\\Z|jet\\s?blue"))


## Select columns from the "narratives" table -----------------------------------

narratives <- ntsb_data$narratives %>%
  select(ev_id, narr_accp) %>%
  group_by(ev_id) %>%
  # some events have more than one narrative but it seems that the difference
  # between the narratives are minor. There should only be one narrative per event.
  slice_head()

# Combine Data ----------------------------------------------------------------

aircraft_events <- left_join(aircraft, events, by = join_by(ev_id)) %>%
  select(ev_id, ev_date, everything()) %>%
  mutate(ev_date = as.Date(ev_date, "%m/%d/%Y"),
         year = year(ev_date),
         month = month(ev_date)) %>%
  relocate(year, month, .after = ev_date) %>%
  left_join(., narratives, by = join_by(ev_id))
  
## combine all similar spellings of each airline carrier ----------------------

aircraft_events <- aircraft_events %>%
  mutate(owner_acft = case_when(
    
    str_detect(owner_acft, "delta air\\s?lines") ~ "delta airlines",
    str_detect(owner_acft, "southwest air\\s?lines") ~ "southwest airlines",
    str_detect(owner_acft, "united air\\s?lines|united") ~ "united airlines",
    str_detect(owner_acft, "american air\\s?lines") ~ "american airlines",
    str_detect(owner_acft, "skywest air\\s?lines") ~ "skywest airlines",
    str_detect(owner_acft, "allegiant air\\s?lines") ~ "allegiant airlines",
    str_detect(owner_acft, "frontier air\\s?lines") ~ "frontier airlines",
    str_detect(owner_acft, "spirit air\\s?lines") ~ "spirit airlines",
    str_detect(owner_acft, "continental air\\s?lines") ~ "continental airlines",
    str_detect(owner_acft, "jet\\s?blue") ~ "jetblue airways",
    TRUE ~ NA
    
  ))

# test --- see if there are any NA values after re-categorization

# 0 observations
test <- aircraft_events %>% filter(is.na(owner_acft))

# Output -----------

save(aircraft_events, file = "aircraft_events.Rdata")


