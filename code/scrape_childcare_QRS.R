# SCRAPE CHILDCARE PROVIDER DATA ------------------------------------------
# This script scrapes html table of QRS providers from Iowa DHS website
# URL: https://dhs.iowa.gov/iqrs/providers
# File is cleaned and indicators calculated

library(tidyverse)
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


# SCRAPE QRS DATA ---------------------------------------------------------

# Function to extract tables with QRS providers 
scrape_QRS_provider_list <- function(URL) {
  scrape_date <- 
    xml2::read_html(URL) %>%
    rvest::html_nodes("div.field-items h2") %>%
    rvest::html_text()
  
  df <- 
    xml2::read_html(URL) %>%
    rvest::html_node("tbody") %>%
    rvest::html_table() %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    mutate(month = str_extract(scrape_date, "[:alpha:]+"),
           year = str_extract(scrape_date, "[:number:]+"))
  
  return(df)
}


# Read Certified Child Development Homes List
QRS_provider_home <- scrape_QRS_provider_list("https://dhs.iowa.gov/iqrs/providers/homes")

# Read Certified Child Development Centers List
QRS_provider_center <- scrape_QRS_provider_list("https://dhs.iowa.gov/iqrs/providers/centers")


# Combine List of Certified Child Development Centers and Homes
QRS_provider_list <- 
  bind_rows(QRS_provider_center, QRS_provider_home) %>%
  mutate(provider_type = ifelse(is.na(provider_last_name), "Centers", "Homes"),
         provider_name = ifelse(is.na(provider_last_name), 
                                provider_name, 
                                paste0(provider_last_name, ", ", provider_first_name)),
         certificate_expiration_date = lubridate::mdy(certificate_expiration_date)) %>%
  select(provider_name, provider_type, county, city, 
         report_year = year, report_month = month,
         qrs_level = current_qrs_level, certificate_expiration_date)


# Save Raw data
write_csv(QRS_provider_list, "data/RAW/QRS/qrs_iowa_monthly.csv")



# CALCULATE INDICATORS ----------------------------------------------------

# Calculate Number of Childcare Providers by County
childcare_providers_county <-
  QRS_provider_list %>%
  # calculate number of providers per county
  mutate(qrs_45 = ifelse(qrs_level %in% 4:5, 1, 0)) %>%
  group_by(county, report_year, report_month, provider_type) %>%
  summarise(number_of_childcare_providers = n(),
            qrs_level_4_5_providers = sum(qrs_45)) 

  
# Calculate Statewide Number of Providers
childcare_providers_statewide <-
  childcare_providers_county %>%
  group_by(report_year, report_month, provider_type) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(county = "Statewide")


# Combine Statewide and County data
childcare_providers_number <-
  bind_rows(childcare_providers_county, childcare_providers_statewide) %>%
  # standardize county names
  standardize_county() %>%
  # create report date (for plotting)
  mutate(report_date = lubridate::ymd(paste(report_year, report_month, 1))) %>%
  select(fips, county, report_year, report_month, report_date, everything())


# Save Data
write_rds(childcare_providers_number, "data/CLEAN/childcare_number_monthly.rds", compress = "xz")


