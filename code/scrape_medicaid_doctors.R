# SCRAPE MEDICAID DOCTORS DATA --------------------------------------------
# This script downloads list of available Dentists and Pediatricians that accept 
# Medicaid from Iowa Department of Human Services as a pdf file
# URL: "https://secureapp.dhs.state.ia.us/providersearche/"
# Files were standardized and combined into a single table

library(tidyverse)
library(rvest)
library(xml2)
library(RSelenium)
library(tabulizer)



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



# DOWNLOAD MEDICAID DOCTORS DATA ------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://secureapp.dhs.state.ia.us/providersearche/"

# The script for scraping is available from Tiancheng `Medicaid_Webscraper.R`
# It will be incorportated here 
# Currently the file was mannually downloaded

# get the date when data was updated
Date_Update <- Sys.Date()


# READ COUNTY FIPS CODES --------------------------------------------------

ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# SCRAPE MEDICAID DOCTORS DATA --------------------------------------------

# Scrape Dentist Data
my_file <- "data/RAW/medicaid_doctors/Dentist Record.pdf"

pages <- tabulizer::get_n_pages(my_file)

my_pages <- list()
for (i in 1:pages) {
  my_pages[[i]] <- 
    tabulizer::extract_tables(file = my_file, pages = i, method = "stream") %>%
    data.frame() %>% 
    tibble() %>%
    mutate(across(everything(), ~ str_replace(.x, "^$", NA_character_))) %>%
    janitor::remove_empty('cols') %>%
    mutate_all(as.character) %>%
    set_names(c(paste("var", 1:ncol(.), sep = "_")))
}

data <-
  bind_rows(my_pages) %>%
  janitor::row_to_names(row_number = 1) %>%
  # convert all values to string
  rownames_to_column() %>%
  mutate(rowname = ifelse(Specialty == '', NA, rowname)) %>%
  fill(rowname) %>%
  group_by(rowname) %>%
  summarise_all(paste, collapse = " ") %>%
  mutate(rowname = as.numeric(rowname)) %>% 
  arrange(rowname) %>%
  select(-rowname) %>%
  # remove trailing NAs
  mutate_all(str_remove, "( NA){1,3}$")

# Standardize Data
dentist_list <-
  data %>%
  janitor::clean_names() %>%
  # extract zip code from addresses
  mutate(zipcode = str_extract(address, "\\d{5}$")) %>%
  # standardize county names 
  standardize_county() %>%
  # select only dentists 
  filter(type == "DENTIST") %>%
  select(fips, county, name, address, zipcode, phone, specialty, accepting_new_clients = accepting)
  
# Save Extracted Data
write_csv(dentist_list, "data/RAW/medicaid_doctors/dentist.csv")


# Scrape Pediatrics Data
my_file <- "data/RAW/medicaid_doctors/Pediatrics Report.pdf"

pages <- tabulizer::get_n_pages(my_file)

my_pages <- list()
for (i in 1:pages) {
  my_pages[[i]] <- 
    tabulizer::extract_tables(file = my_file, pages = i, method = "stream") %>%
    data.frame() %>% 
    tibble() %>%
    mutate(across(everything(), ~ str_replace(.x, "^$", NA_character_))) %>%
    janitor::remove_empty('cols') %>%
    mutate_all(as.character) %>%
    set_names(c(paste("var", 1:ncol(.), sep = "_")))
}

data <-
  bind_rows(my_pages) %>%
  janitor::row_to_names(row_number = 1) %>%
  # convert all values to string
  rownames_to_column() %>%
  mutate(rowname = ifelse(Specialty == '', NA, rowname)) %>%
  fill(rowname) %>%
  group_by(rowname) %>%
  summarise_all(paste, collapse = " ") %>%
  mutate(rowname = as.numeric(rowname)) %>% 
  arrange(rowname) %>%
  select(-rowname) %>%
  # remove trailing NAs
  mutate_all(str_remove, "( NA){1,3}$")

# Standardize Data
pediatrics_list <-
  data %>%
  janitor::clean_names() %>%
  # extract zip code from addresses
  mutate(zipcode = str_extract(address, "\\d{5}$")) %>%
  # standardize county names 
  standardize_county() %>%
  select(fips, county, name, address, zipcode, phone, specialty, type)

# Save Extracted Data
write_csv(pediatrics_list, "data/RAW/medicaid_doctors/pediatrics.csv")



# CALCULATE INDICATORS ----------------------------------------------------

dentist_count <-
  dentist_list %>%
  # remove duplicated dentist entries
  # eliminated address in case same dentists works in more than 1 offices
  distinct(fips, county, name, specialty, accepting_new_clients) %>%
  # find if there is any duplicated dentist entry with different acceptance 
  add_count(name, fips) %>%
  # there were 2 instances for 11 dentists (total of 22 cases)
  # one of the instance was "Unkonwn" for every all 11 dentists
  # while the other instance was Yes, No, or Conditional
  # decided to drop all duplicates with Unknown acceptance
  filter(!(n > 1 & accepting_new_clients == "Unknown")) %>%
  # calculate number of dentists per county
  group_by(fips, county, accepting_new_clients) %>%
  summarise(dentsit_count = n()) %>%
  ungroup()

# Save Data
dentist_count %>%
  mutate(date = Date_Update) %>%
  write_rds("data/CLEAN/dentists.rds", compress = "xz")


pediatrics_count <- 
  pediatrics_list %>%
  distinct(fips, county, name, specialty, type) %>%
  group_by(fips, county, specialty, type) %>%
  summarise(pediatrics_count = n()) %>%
  ungroup() 
  
# Save Data
pediatrics_count %>%
  mutate(date = Date_Update) %>%
  write_rds("data/CLEAN/pediatrics.rds", compress = "xz")






