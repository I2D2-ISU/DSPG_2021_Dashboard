# SCRAPE CHILDCARE PROVIDER DATA ------------------------------------------
# This script downloads childcare providers data as an excel files from Iowa DHS
# URL: https://ccmis.dhs.state.ia.us/clientportal/ProviderSearch.aspx
# File is cleaned and indicators calculated

library(tidyverse)
library(RSelenium)
library(rvest)
library(xml2)



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



# DOWNLOAD CHILD ABUSE ----------------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://ccmis.dhs.state.ia.us/clientportal/ProviderSearch.aspx"

# TO BE DEVELOPED
# Need to use Rselenium for downloading excel file 
# Currently data was downloaded manually

# Alternative source is https://dhs.iowa.gov/iqrs/providers
# NOTE: this source does not list child cares with no QRS (qrs = 0) raitings



# READ COUNTY POPULATION --------------------------------------------------

county_population <- read_csv("data/RAW/ACS/pop5yr_acs.csv")


# Calculate population of children under 6 years
county_population_under_6 <-
  county_population %>%
  filter(variable %in% c("B09001_003", "B09001_004", "B09001_005")) %>%
  group_by(fips = GEOID, county_name = NAME, year) %>%
  summarise(population_under_6 = sum(estimate)) %>%
  mutate(county_name = ifelse(fips == 19, "Statewide", county_name)) %>%
  mutate(county_CAP = standardize_string(county_name)) %>%
  ungroup()

# Create standardize IA County list
ia_counties <- 
  county_population_under_6 %>%
  distinct(county_name, fips, county_CAP)


# READ QRS DATA -----------------------------------------------------------

data <- readxl::read_xls("data/RAW/QRS/qrs_iowa.XLS")


# Calculate Statewide Capacity
childcare_capacity <-
  data %>%
  # calculate available spots per county
  group_by(COUNTY) %>%
  mutate(qrs_45 = ifelse(`PROVIDER QRS RATING` %in% 4:5, 1, 0)) %>%
  summarise(capacity = sum(as.numeric(`PROVIDER CAPACITY`), na.rm = TRUE),
            number_of_childcare_providers = n(),
            qrs_level_4_5_providers = sum(qrs_45)) %>%
  ungroup() %>%
  # select only IA counties 
  mutate(county_CAP = standardize_string(COUNTY)) %>%
  filter(county_CAP %in% ia_counties$county_CAP) %>%
  # calculate statewide capacity
  janitor::adorn_totals("row", name = "Statewide", fill = "STATEWIDE") %>%
  select(county_CAP, number_of_childcare_providers, qrs_level_4_5_providers, capacity) %>% 
  tibble()


# Calculate Indicator 
childcare_indicator <-
  childcare_capacity %>% 
  # merge the latest (2019) child population data since there is no 2020
  left_join(county_population_under_6 %>% filter(year == 2019)) %>%
  # replace year with year corresponding to childcare data year
  mutate(year = 2020) %>%
  # calculate indicator
  mutate(childcare_availibility_index = population_under_6 / capacity,
         childcare_desert = case_when(
           population_under_6 >= 50 & number_of_childcare_providers == 0 ~ "Yes",
           population_under_6 >= 50 & childcare_availibility_index >= 3 ~ "Yes",
           TRUE ~ "No")) %>%
  select(fips, county = county_name, population_under_6,
         number_of_childcare_providers, qrs_level_4_5_providers, capacity, 
         childcare_availibility_index, childcare_desert)


# Save Data
write_rds(childcare_indicator, "data/CLEAN/childcare_indicator.rds", compress = "xz")


