# SCRAPE ARREST DATA ------------------------------------------------------
# This script obtains Arrest Demographic data using FBI Crime Data API
# URL: https://crime-data-explorer.fr.cloud.gov/pages/docApi
# File is cleaned and indicators calculated

library(tidyverse)
library(httr)
library(censusapi)



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



# READ COUNTY POPULATION --------------------------------------------------

county_population <- read_csv("data/RAW/ACS/pop5yr_acs.csv")


# Create standardize IA County list
ia_counties <- 
  county_population %>%
  distinct(fips = GEOID, county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# GET DATA ----------------------------------------------------------------

# Get ACS 5yr Estimate for under 18
population_under_18 <- list()

for (i in 2009:2019) {
  population_under_18[[i]] <-
    getCensus(
      name = "acs/acs5",
      vintage = i, 
      vars = c("NAME", "B09001_001E", "B09001_002E"),
      region = "county:*", 
      regionin = "state:19") %>%
    mutate(year = i)
}

# Standardize data
population_under_18 <- 
  bind_rows(population_under_18) %>%
  unite(fips, state, county, sep = "") %>%
  mutate(fips = as.numeric(fips)) %>%
  left_join(ia_counties) %>%
  select(fips, county = county_name, year, 
         population_under_18 = B09001_001E, 
         population_under_18_household = B09001_002E) %>%
  tibble()


# Base URL of the FBI API
base_url <- "https://api.usa.gov/crime/fbi/sapi/"

# Get API Key for data.gov
api_key <- sprintf("?API_KEY=%s", Sys.getenv("FBI_KEY"))


# Get Agency Participation data
download.file(sprintf("%sapi/participation/dl/states/IA%s", base_url, api_key),
              "data/RAW/juvenile_arrests/agency_participation.csv")


# Get list of UCR Agencies in Iowa
response <- GET(sprintf("%sapi/agencies/byStateAbbr/IA%s", base_url, api_key))
response <- jsonlite::fromJSON(rawToChar(response$content))
iowa_agencies <- response$results


# Get Arrest by Age and Sex
YEAR_since = 2000
YEAR_until = 2019

data <- list()
for (i in c("male", "female")) {
  df <- list()
  for (j in iowa_agencies$ori) {
    url_section <- sprintf("api/data/arrest/agencies/%s/all/%s/%s/%s",
                           j, i, YEAR_since, YEAR_until)
    response <- GET(paste0(base_url, url_section, api_key))
    if (response$status_code == 200) {
      response <- jsonlite::fromJSON(rawToChar(response$content))
      results <- response$results
      if (!is_empty(results)) {
        df[[j]] <-
          results %>%
          gather("age", "arrests", starts_with("range")) %>%
          mutate(sex = i,
                 ori = j) %>%
          select(ori, sex, year = data_year, age, arrests) 
      }
    }
  }
  data[[i]] <- bind_rows(df)
}

# Save raw data
bind_rows(data) %>%
  write_csv("data/RAW/juvenile_arrests/arrests.csv")

write_csv(iowa_agencies, "data/RAW/juvenile_arrests/iowa_agencies.csv")



# CALCULATE INDICATORS ----------------------------------------------------

# Standardize County Names for Agencies
iowa_agencies_by_county <-
  iowa_agencies %>% 
  separate(county_name, into = 'county', sep = ";", extra = "drop", remove = TRUE) %>%
  standardize_county() %>%
  # discard artificially made stateswide records
  filter(fips != 19) %>%
  filter(!is.na(fips)) %>%
  select(ori, fips, county)
  

# Combine Juvenile Arrest Data by County
juvenile_arrests_county <-
  bind_rows(data) %>%
  # # calculate number of arrests for age range 10 to 17
  # mutate(juvenile = ifelse(str_extract(age, "\\d{2}$") %in% 10:17, 'juvenile_arrest_count', "rest")) %>%
  # calculate number of arrests for age under 18
  mutate(juvenile = ifelse(str_extract(age, "\\d{2}") < 18, 'juvenile_arrest_count', "rest")) %>%
  group_by(ori, year, sex, juvenile) %>%
  summarise(arrests = sum(arrests)) %>%
  left_join(iowa_agencies_by_county) %>%
  group_by(fips, county, year, juvenile) %>%
  summarise(arrests = sum(arrests)) %>%
  spread(juvenile, arrests) %>%
  mutate(total_arrest_count = juvenile_arrest_count + rest) %>%
  select(-rest) %>%
  left_join(population_under_18 %>% 
              select(-population_under_18_household)) %>%
  ungroup()
  
# Calculate Statewide Number of Juvenile Arrests
juvenile_arrests_statewide <-
  juvenile_arrests_county %>%
  group_by(year) %>%
  summarise(fips = 19,
            county = "Statewide",
            juvenile_arrest_count = sum(juvenile_arrest_count),
            total_arrest_count = sum(total_arrest_count),
            population_under_18 = sum(population_under_18))

# Combine Statewide and County data
juvenile_arrests <-
  bind_rows(juvenile_arrests_county, juvenile_arrests_statewide) %>%
  # standardize county names
  standardize_county() %>%
  # calculate indicator
  mutate(juvenile_arrest_index = juvenile_arrest_count / population_under_18 * 100000,
         juvenile_arrest_percent = juvenile_arrest_count / total_arrest_count)
  

# Save Data
write_rds(juvenile_arrests, "data/CLEAN/juvenile_arrests.rds", compress = "xz")


