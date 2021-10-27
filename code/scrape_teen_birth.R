# SCRAPE TEEN BIRTH DATA --------------------------------------------------
# This script downloads Teen Birth data as a csv file from IDPH website
# URL: https://tracking.idph.iowa.gov/people-community/Reproduction-and-Birth/Birth-Rate-Mothers-Under-20
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



# DOWNLOAD TEEN BIRTH DATA ------------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://tracking.idph.iowa.gov/people-community/Reproduction-and-Birth/Birth-Rate-Mothers-Under-20"

# UNDER DEVELOPMENT
# The file was manually downloaded


# READ COUNTY FIPS CODES --------------------------------------------------

ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# READ TEEN BIRTH DATA ----------------------------------------------------

# Read Statewide Data
data <- read_csv("data/RAW/teen_birth/State_Birth_Rate_-_Mothers_Under_20_data.csv")

# Transform Data
teen_birth_state <-
  data %>%
  # transformt to wide format
  spread(`Measure Names`, `Measure Values`) %>%
  set_names("year", "county", "count", "rate") %>%
  mutate(county = "Statewide")


# Read County Data
data <- read_csv("data/RAW/teen_birth/County_Birth_Rate_-_Mothers_Under_20_data.csv")

# Transform Data
teen_birth_county <-
  data %>%
  # transform to wide format
  spread(`Measure Names`, `Measure Values`) %>%
  select(year = Year, county = County, 
         count = `Live Birth Count`, 
         rate = `Live Birth Rate`) 


# Combine and Standardize Data
teen_birth <-
  bind_rows(teen_birth_county, teen_birth_state) %>%
  standardize_county() %>%
  select(fips, county, year, 
         teen_birth_count = count, 
         teen_birth_rate = rate)


# Save Data
write_rds(teen_birth, "data/CLEAN/teen_birth.rds", compress = "xz")


