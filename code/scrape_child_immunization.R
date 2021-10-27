# SCRAPE IMMUNIZATION DATA ------------------------------------------------
# This script downloads Children Immunization data as a csv file from IDPH website
# URL: https://tracking.idph.iowa.gov/Health/Immunization/Childhood-Immunizations/Childhood-Immunization-Data
# Files were standardized and combined into a single table

library(tidyverse)
library(rvest)
library(xml2)
library(RSelenium)



# FUNCTIONS ---------------------------------------------------------------

# Function to standardize text for merging
standardize_string <- function(x) {
  x %>%
    str_squish() %>%
    str_remove_all("[:punct:]") %>%
    str_remove_all(" ") %>%
    str_to_upper()
}


# Function to standardize county names
standardize_county <- function(df) {
  df %>%
    mutate(county_CAP = standardize_string(county)) %>%
    left_join(ia_counties) %>%
    mutate(county = ifelse(!is.na(county_name), county_name, county)) %>%
    select(fips, names(df))
}



# DOWNLOAD IMMUNIZATION DATA ----------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://tracking.idph.iowa.gov/Health/Immunization/Childhood-Immunizations/Childhood-Immunization-Data"

# UNDER DEVELOPMENT
# The file was manually downloaded


# READ COUNTY FIPS CODES --------------------------------------------------

ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# READ IMMUNIZATION DATA --------------------------------------------------

data <- 
  fs::dir_ls("data/RAW/immunization/", full.names = TRUE) %>%
  map_df(read_csv, .id = "file_name") %>%
  janitor::clean_names()


# Standardize Data
immunization_county <-
  data %>%
  # extract year of measurement
  mutate(year = str_extract(file_name, "\\d{4}")) %>%
  select(county, year, popullation_census = pop, doses_count = number_of_doses, immunization_percent = rate_of_immunization) 


# Calculate Statewide Rates
immunization_statewide <-
  immunization_county %>%
  group_by(year) %>%
  summarise(popullation_census = sum(popullation_census),
            doses_count = sum(doses_count)) %>%
  ungroup() %>%
  mutate(county = "Statewide",
         immunization_percent = doses_count /  popullation_census)


# Combine Statewide and County Data 
immunization <-
  bind_rows(immunization_county, immunization_statewide) %>%
  # standardize county names 
  standardize_county()


# Save Data
write_rds(immunization, "data/CLEAN/child_immunization.rds", compress = "xz")


