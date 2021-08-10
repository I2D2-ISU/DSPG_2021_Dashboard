# SCRAPE CHILD DENTAL SCREENING DATA --------------------------------------
# This script scrapes School Dental Screening Audit Report from pdf files on IDPH website
# URL: https://idph.iowa.gov/ohds/reports
# Files were standardized and combined into a single table

library(tidyverse)
library(rvest)
library(xml2)
library(pdftools)



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



# READ AUXILIARY DATA -----------------------------------------------------

# Read county fips codes
ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))


# Read variable names
vars <- read_csv("data/dental_service_variable_definition.csv") %>%
  filter(var %in% names(data))



# GET URL FOR PDF FILES ---------------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://idph.iowa.gov/ohds/reports"

# Extracting links of excel files with dental screening audit report 
xml2::read_html(URL) %>%
  rvest::html_nodes("#dnn_ctr2388_HtmlModule_lblContent li a") %>%
  rvest::html_attr("href") %>%
  url_absolute(., URL) %>%
  str_subset("School%20Screening") -> pdf_file_links



# SCRAPE CHILD DENTAL AUDIT REPORT ----------------------------------------

# Function to parse the dental screening audit table 
# NOTE: this table includes results of students enrolled in K or 9th grade
parse_dental_screening_table <- function(df) {
  df %>%
    filter(y > y[text == "Obvious"]) %>%
    filter(y > y[text == "Needs"]) %>%
    arrange(y, x) %>%
    mutate(x = ifelse(space == "TRUE", NA, x)) %>%
    fill(x, .direction = "up") %>%
    group_by(y, x) %>%
    summarise(text = paste(text, collapse = " ")) %>%
    ungroup() %>%
    arrange(y, x) %>%
    add_count(y) %>%
    filter(n == median(n)) %>%
    group_by(y) %>%
    mutate(name = paste0("var_", 1:n())) %>%
    select(text, name) %>%
    spread(name, text) %>%
    mutate(var_1 = str_remove(var_1, " County Total"))
}


# Function to scrape and combine county level data from multiple detnal audit tables
scrape_dental_screening_table <- function(URL, PAGES = 4:5) {
  # create dummy list
  df <- vector("list", length(PAGES))
  # parse table on each page
  for (i in seq_along(df)) {
    print(i)
    df[[i]] <- pdftools::pdf_data(URL) %>%
      pluck(PAGES[i]) %>%
      parse_dental_screening_table() %>%
      mutate_all(as.character)
  }
  # combine parsed data
  df <- bind_rows(df) %>%
    # add corresponding school year
    mutate(school_year = basename(URL) %>% 
             str_extract("\\d{4}-\\d{4}")) %>%
    ungroup() %>%
    select(school_year, county = var_1, starts_with("var")) 
  # return data frame
  return(df)
}


# Function to parse the statewide dental screening data by grade
# NOTE: this function extracts results of students enrolled in Kindergarten only
scrape_dental_screening_table_statewide <- function(URL) {
  tabulizer::extract_tables(URL, pages = 2) %>%
    pluck(1) %>%
    as_tibble() %>%
    filter(V1 == "K") %>%
    set_names(c("grade", vars_statewide)) %>%
    mutate(school_year = str_extract(basename(URL), "\\d{4}-\\d{4}"),
           county = "Statewide",
           fips = 19) %>%
    select(school_year, fips, county, everything())
}


# Scrape and combine dental audit data by county
data <- map_dfr(pdf_file_links, scrape_dental_screening_table)


# Scrape and combine statewide dental audit data for K

vars_statewide <- vars %>% filter(!var %in% c('var_7', 'var_13', 'var_17', 'var_19')) %>%
  pull(col_name)

data_statewide <- map_dfr(pdf_file_links, scrape_dental_screening_table_statewide)



# STANDARDIZE & SAVE DATA -------------------------------------------------

# Standardize columns and county names
dental_audit_k9 <- 
  data %>% 
  # rename variables
  # NOTE: column renaming assumes same table structure (column sequence) as in 2018-2019
  select(school_year, county, vars$var) %>%
  set_names('school_year', 'county', vars$col_name) %>%
  # standardize county names 
  mutate(county = ifelse(county == "State Total", "Statewide", county)) %>%
  standardize_county() %>%
  select(school_year, fips, county, everything())


# Save Data
write_csv(dental_audit_k9, "data/RAW/K_dental_screening/K9_dental_screening_data.csv")
write_csv(data_statewide, "data/RAW/K_dental_screening/K_dental_screening_statewide_data.csv")



# ---------------- UNDER DEVELOPMENT ------------------

# ---------------------------- SUPPRESSED VALUE RECALCULATE

a <- scrape_dental_screening_table(pdf_file_links[2])

a %>%
  select(county, var_4, var_5, var_6, var_7) %>%
  gather(key, measure, starts_with("var")) %>%
  # impute based on county totals
  mutate(value = parse_number(measure),
         group = ifelse(key == "var_7", "total", "summand")) %>%
  group_by(county, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(county) %>%
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value),
         # impute those with two suppressed values sum to 2
         value = ifelse(is.na(value) & count_missing == 2 & estimate == 2, 1, value)) %>%
  select(county, key, value) %>%
  # impute based on state totals
  mutate(group = ifelse(str_detect(county, "Total"), "total", "summand")) %>%
  group_by(key, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(key) %>%
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value)) %>%
  # if there are still missing check if diff of missing values equal to number of missing
  mutate(count_missing = sum(is.na(value)),
         estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         value = ifelse(is.na(value) & count_missing == estimate, 1, value)) %>%
  # select(county, key, value) %>%
  select(county, key, value) %>% 
  spread(key, value)
  

a %>%
  select(county, var_8, var_9, var_10, var_11, var_12, var_13) %>%
  gather(key, measure, starts_with("var")) %>%
  # impute based on county totals
  mutate(value = parse_number(measure),
         group = ifelse(key == "var_13", "total", "summand")) %>%
  group_by(county, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(county) %>% 
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value),
         # impute those with two suppressed values sum to 2
         value = ifelse(is.na(value) & count_missing == 2 & estimate == 2, 1, value),
         # impute those with three suppressed values sum to 3
         value = ifelse(is.na(value) & count_missing == 3 & estimate == 3, 1, value),
         # impute those with four suppressed values sum to 4
         value = ifelse(is.na(value) & count_missing == 4 & estimate == 4, 1, value)) %>%
  select(county, key, value) %>%
  # impute based on state totals
  mutate(group = ifelse(str_detect(county, "Total"), "total", "summand")) %>%
  group_by(key, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(key) %>%
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value)) %>%
  # if there are still missing check if diff of missing values equal to number of missing
  mutate(count_missing = sum(is.na(value)),
         estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         value = ifelse(is.na(value) & count_missing == estimate, 1, value)) %>%
  # select(county, key, value) %>%
  select(county, key, value) %>% 
  spread(key, value)



a %>%
  select(county, var_3, var_14, var_15, var_16) %>%
  gather(key, measure, starts_with("var")) %>%
  # impute based on county totals
  mutate(value = parse_number(measure),
         group = ifelse(key == "var_16", "total", "summand")) %>%
  group_by(county, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(county) %>% 
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value),
         # impute those with two suppressed values sum to 2
         value = ifelse(is.na(value) & count_missing == 2 & estimate == 2, 1, value)) %>%
  select(county, key, value) %>%
  # impute based on state totals
  mutate(group = ifelse(str_detect(county, "Total"), "total", "summand")) %>%
  group_by(key, group) %>%
  mutate(count_missing = sum(is.na(value))) %>%
  group_by(key) %>%
  # recalculate suppressed values 
  mutate(estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         # impute those with only one suppressed value
         value = ifelse(is.na(value) & count_missing == 1, estimate, value)) %>%
  # if there are still missing check if diff of missing values equal to number of missing
  mutate(count_missing = sum(is.na(value)),
         estimate = value[group == 'total'] - sum(value[group=="summand"], na.rm = TRUE),
         value = ifelse(is.na(value) & count_missing == estimate, 1, value)) %>%
  # select(county, key, value) %>%
  select(county, key, value) %>% 
  spread(key, value)


a %>%
  select(county, var_2, var_3, var_18) %>%
  mutate_at(vars(starts_with("var")), parse_number) %>%
  mutate(var_18 = ifelse(is.na(var_18), var_2 - var_3, var_18))




# CALCULATE FAST IDNDICATORS ----------------------------------------------

# Read K Assessment Data
k_assessment <- read_csv("data/RAW/K_assessment/K_assessment_data.csv")


# Calculate Statewide Percentage 
k_assessment_statewide <-
  k_assessment %>%
  group_by(Year, Test) %>%
  summarise(Percent_Met_Benchmark = sum(Number_Met_Benchmark, na.rm = TRUE)/
              sum(Number_Tested, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fips = 19, 
         county = "Statewide")


# Combine ECI Areas with Corresponding Counties 
k_assessment_FAST  <- 
  k_assessment %>%
  # aggregate data into ECI area
  group_by(ECI_Number, ECI_Area_Name, Test, Year) %>%
  summarise(Percent_Met_Benchmark = sum(Number_Met_Benchmark, na.rm = TRUE)/
              sum(Number_Tested, na.rm = TRUE)) %>%
  # assign each area average value to corresponding county
  left_join(eci_areas %>% select(1:3)) %>%
  # combine statewide and county data
  bind_rows(k_assessment_statewide) %>%
  # choose results for FAST assessment only
  filter(Test == "FAST") %>%
  select(fips, county, everything()) %>%
  arrange(fips) %>%
  janitor::clean_names() %>%
  mutate(year = factor(year))


# Save K Assessment Data for Dashboard
write_rds(k_assessment_FAST, "data/CLEAN/k_assessment.rds", compress = "xz")

