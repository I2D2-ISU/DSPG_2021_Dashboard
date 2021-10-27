# SCRAPE CHILD DENTAL SERVICE DATA ----------------------------------------
# This script downloads 0 to 5 years Children Dental Services Medicaid data as a csv file from IDPH website
# URL: https://tracking.idph.iowa.gov/Health/Oral-Health/Child-Dental-Services-Medicaid-Data
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



# DOWNLOAD CHILD DENTAL SERVICE DATA --------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://tracking.idph.iowa.gov/Health/Oral-Health/Child-Dental-Services-Medicaid-Data"

# UNDER DEVELOPMENT
# The file was manually downloaded


# READ COUNTY FIPS CODES --------------------------------------------------

ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# READ CHILD DENTAL SERVICE DATA ------------------------------------------

data <- 
  fs::dir_ls("data/RAW/child_dental_service/", full.names = TRUE) %>%
  map_df(read_csv, .id = "file_name") %>%
  janitor::clean_names()


# Standardize Data
dental_service_county <-
  data %>%
  # extract year of measurement
  mutate(year = str_extract(file_name, "\\d{4}")) %>%
  select(county, year, measure, measure_count, medicaid_enrolled) 


# Calculate Statewide Rates
dental_service_statewide <-
  dental_service_county %>%
  group_by(year) %>%
  summarise(measure = first(measure),
            measure_count = sum(measure_count),
            medicaid_enrolled = sum(medicaid_enrolled)) %>%
  ungroup() %>%
  mutate(county = "Statewide")


# Combine Statewide and County Data 
dental_service <-
  bind_rows(dental_service_county, dental_service_statewide) %>%
  # standardize county names 
  standardize_county() %>%
  mutate(measure_percent = measure_count / medicaid_enrolled)


# Save Data
write_rds(dental_service, "data/CLEAN/child_dental_service.rds", compress = "xz")


