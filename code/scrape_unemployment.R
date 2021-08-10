# SCRAPE UNEMPLOYMENT DATA ------------------------------------------------
# This script downloads unemployment statistics as a csv file from Iowa 
# Workforce Development (IWD) website
# URL: https://www.iowalmi.gov/local-area-unemployment-statistics
# Files are cleaned and combined into a single table

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



# DOWNLOAD UNEMPLOYMENT DATA ----------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://www.iowalmi.gov/local-area-unemployment-statistics"

# Extracting links of excel files with child abuse data
xml2::read_html(URL) %>%
  rvest::html_element("div.main-container.container") %>%
  rvest::html_elements("a.btn.iwd-btn-blue") %>%
  rvest::html_attr("href") %>%
  url_absolute(., URL) %>%
  str_subset("csv$") -> csv_file_link
  
  
# Function to download and save excel files
download_excel_files <- function(file_link, folder) {
  file_name <- str_replace_all(basename(file_link), "%20", "_")
  file_path <- paste0(folder, file_name)
  download.file(file_link, file_path, mode = "wb")
}

# Download and save on local folder excel files 
map(csv_file_link, download_excel_files, folder = "data/RAW/unemployment/")



# READ COUNTY FIPS CODES --------------------------------------------------

ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# READ UNEMPLOYMENT DATA --------------------------------------------------

data <- read_csv("data/RAW/unemployment/Iowa_LAUS_Data_2021-06.csv")


# Standardize Data
unemployment <-
  data %>%
  # select only county and state level data
  filter(AREATYNAME %in% c("County", "State")) %>%
  # select only non-preliminary measurements
  filter(PRELIM == 0) %>%
  # standardize count names
  mutate(county_CAP = standardize_string(AREANAME),
         # remove "County" from the end of the county names
         county_CAP = str_remove(county_CAP, "COUNTY$"),
         # replace "State" with statewide
         county_CAP = ifelse(AREATYNAME == "State", "STATEWIDE", county_CAP)) %>%
  left_join(ia_counties) %>%
  # convert month name to numeric month
  mutate(month = factor(MONTH, levels = month.name)) %>%
  select(fips, 
         county = county_name, 
         year = YEAR,
         month,
         adjusted = ADJUSTED,
         laborforce = LABORFORCE,
         employment = EMP,
         unemployment = UNEMP,
         unemployment_rate = UNEMPRATE) %>%
  # select only adjusted values for statewide rates
  filter(!(fips == 19 & adjusted == 0))


# Save Data
write_rds(unemployment, "data/CLEAN/unemployment.rds", compress = "xz")


