# SCRAPE CHILD ABUSE DATA -------------------------------------------------
# This script downloads child abuse data as excel files from Iowa DHS
# URL: https://dhs.iowa.gov/reports/child-abuse-statistics
# Files are cleaned and combined into a single table

library(tidyverse)
library(rvest)
library(xml2)



# FUNCTIONS ---------------------------------------------------------------

# Function to read excel files
read_excel_files <- function(FILE_PATH, SHEET = 1) {
  
  # read first 10 rows of the first column
  temp <- 
    readxl::read_xls(FILE_PATH, 
                     sheet = SHEET, 
                     range = "A1:A10", 
                     col_names = FALSE) %>% 
    pull(1) %>%
    str_squish() %>% str_to_lower()
  
  # find the row where data starts
  first_row = which(temp == "county")
  
  # read the excel data from the row where data starts
  data <- readxl::read_xls(FILE_PATH, 
                           sheet = SHEET, 
                           skip = (first_row - 1), 
                           col_types = "text", 
                           .name_repair = janitor::make_clean_names)
  
  return(data)
}


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
URL  <- "https://dhs.iowa.gov/reports/child-abuse-statistics"

# Extracting links of excel files with child abuse data
xml2::read_html(URL) %>%
  rvest::html_nodes(".even li a") %>%
  rvest::html_attr("href") %>%
  url_absolute(., URL) %>%
  str_subset("xls$")-> excel_file_links


# Function to download and save excel files
download_excel_files <- function(file_link, folder) {
  file_path <- paste0(folder, basename(file_link))
  download.file(file_link, file_path, mode = "wb")
}

# Download and save on local folder excel files 
map(excel_file_links, download_excel_files, folder = "data/RAW/child_abuse/")



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


# READ AND COMBINE AGE DATA -----------------------------------------------

# Read "Age" (sheet #5) from Child Abuse files
data <- 
  fs::dir_ls("data/RAW/child_abuse/", full.names = TRUE) %>%
  map_df(read_excel_files, SHEET = 5, .id = "file_name") 


# Standardize Data
child_abuse_age <- 
  data %>%
  # remove redundant rows with meta info 
  filter(!(is.na(dhs_service_area) & !str_detect(county, "Missing") | is.na(county))) %>%
  gather(age, count, -(1:4)) %>% 
  # remove percent of kids
  filter(!str_detect(age, "percent")) %>% 
  # standardize labels of age categories
  mutate(age = case_when(
    str_detect(age, "5")  ~ "5 or younger",
    str_detect(age, "6")  ~ "6 to 10",
    str_detect(age, '11') ~ "11 or older",
    TRUE ~ age)) %>% 
  filter(!is.na(count)) %>%
  # correct missing county name
  mutate(county = ifelse(str_detect(county, "Missing"), "Missing County", county)) %>%
  # add year of measurement
  mutate(year = parse_number(file_name),
         count = as.numeric(count)) %>%
  select(county, year, everything(), -file_name) %>%
  # standardize county names 
  standardize_county()


# Calculate Indicator 
child_abuse_age_indicator <-
  child_abuse_age %>%
  # compute statewide values
  group_by(year, age) %>%
  summarise(count = sum(count),
            fips = 19,
            county = "Statewide") %>%
  ungroup() %>%
  bind_rows(child_abuse_age) %>%
  select(names(child_abuse_age)) %>%
  left_join(county_population_under_6, by = c("fips", "county" = "county_name", "year")) %>%
  # calculate incidence of child abuse per 1,000 children
  mutate(child_abuse_per_1000 = round(count / population_under_6 * 1000, 2)) %>%
  select(everything(), -county_CAP)


# Save Data
write_rds(child_abuse_age_indicator, "data/CLEAN/child_abuse_age.rds", compress = "xz")



# READ AND COMBINE ABUSE TYPE DATA ----------------------------------------

# Read "Type of Abuse" (sheet #4) from Child Abuse files
data <- 
  fs::dir_ls("data/RAW/child_abuse/", full.names = TRUE) %>%
  map_df(read_excel_files, SHEET = 4, .id = "file_name") 


# Standardize Data
child_abuse_type <-
  data %>%
  # remove redundant rows with meta info 
  filter(!(is.na(dhs_service_area) & !str_detect(county, "Missing") | is.na(county))) %>%
  gather(type, count, -file_name, - county, -dhs_service_area, -judicial_district) %>%
  # standardize labels of age categories
  mutate(type = str_remove(type, "_\\d$")) %>%
  mutate(type = case_when(
    str_detect(type, "dcc")  ~ "Denial of Critical Care (Neglect)",
    str_detect(type, "neglect")  ~ "Denial of Critical Care (Neglect)",
    str_detect(type, 'physical') ~ "Physical Abuse",
    str_detect(type, 'sexual') ~ "Sexual Abuse",
    str_detect(type, 'trafficking') ~ "Child Sex Trafficking",
    str_detect(type, 'access_to_obscene_materials') ~ "Allows Access to Obscene Materials",
    str_detect(type, 'access_by_registered_sex') ~ "Allows Access by Registered Sex Offender",
    str_detect(type, 'bestiality') ~ "Bestiality in the Presence of a Minor",
    str_detect(type, 'cohabitation') ~ "Cohabitation with a Registered Sex Offender",
    str_detect(type, 'presence_of_illegal_drugs') ~ "Presence of Illegal Drugs in Child's System (PID)",
    str_detect(type, 'manufacturing') ~ "Exposure to Manufacturing of Meth",
    str_detect(type, 'pid') ~ "Presence of Illegal Drugs in Child's System (PID)",
    str_detect(type, 'prostitution') ~ "Prostitution of a Child",
    str_detect(type, 'dangerous_substance') ~ "Dangerous Substance",
    str_detect(type, 'mental_injury') ~ "Mental Injury",
    str_detect(type, 'other') ~ "Other",
    str_detect(type, 'total') ~ "Total",
    TRUE ~ type)) %>%
  # correct missing county name
  mutate(county = ifelse(str_detect(county, "Missing"), "Missing County", county)) %>%
  # add year of measurement
  mutate(year = parse_number(file_name)) %>%
  # round the floating part of the counts
  mutate(count = round(as.numeric(count), 2)) %>%
  # remove redundant blank cells for count
  filter(!is.na(count)) %>%
  select(county, year, everything(), -file_name) %>%
  # standardize county names 
  standardize_county()


# Calculate Statewide Values 
child_abuse_type_indicator <-
  child_abuse_type %>%
  group_by(year, type) %>%
  summarise(count = sum(count),
            fips = 19,
            county = "Statewide") %>%
  ungroup() %>%
  bind_rows(child_abuse_type) %>%
  select(names(child_abuse_type)) %>%
  arrange(fips, year)


# Save standardize data
write_rds(child_abuse_type_indicator, "data/CLEAN/child_abuse_type.rds", compress = "xz")


