# SCRAPE CHILDCARE RATE & OTHER DATA --------------------------------------
# This script scrapes 2020 child care data from pdf files from ICCR&R
# URL: https://iowaccrr.org/data/2020-county-data-sheets/
# Scraped data are cleaned and combined into 4 tables

library(tidyverse)
library(rvest)
library(xml2)
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


# Function for scraping data from Child Care pdfs - Table 1
scrape_child_care_rates <- function(URL, PAGE = 1) {
  tabulizer::extract_tables(file = URL, pages = PAGE, method = "stream") %>%
    purrr::pluck(1) %>%
    data.frame() %>%
    rownames_to_column() %>%
    mutate(group = case_when(
      row_number() %in% max(rowname):(as.numeric(max(rowname))-1) ~ "C",
      row_number() %in% (as.numeric(max(rowname))-2):(as.numeric(max(rowname))-3) ~ "B",
      TRUE ~ "A")) %>%
    group_by(group) %>%
    summarise(across(.cols = X1:X8, ~ paste(.x, collapse = " "))) %>%
    mutate(across(everything(), str_squish)) %>%
    select(-group) %>%
    janitor::row_to_names(1) %>%
    gather("Age", "Cost", -1) %>%
    set_names(c("Facility_Type", "Age", "Cost_average_per_week")) %>%
    mutate(County = county,
           Cost = parse_number(Cost_average_per_week))
}


# Function for scraping data from Child Care pdfs - Table 2
scrape_child_care_counts <- function(URL) {
  pdftools::pdf_data(pdf = URL) %>%
    purrr::pluck(1) %>%
    filter(y > 530 & y < 750 & x < 320) %>%
    mutate(group = (round(y/15)*15)) %>%
    arrange(group, x) %>%
    group_by(group) %>%
    summarise(text = paste(text, collapse = " ")) %>%
    mutate(label = str_extract(text, ".*(?= \\d{1,2}%$)"),
           percent = str_extract(text, "\\d{1,2}%$"),
           value = parse_number(percent)/100) %>%
    select(label:value) %>%
    mutate(County = county)
}

# Function for scraping data from Child Care pdfs - Table 3
scrape_program_spaces_percent <- function(URL) {
  pdftools::pdf_data(pdf = URL) %>%
    purrr::pluck(1) %>%
    filter(y > 450 & y < 685 & x > 350) %>%
    mutate(group = (round(y/17)*17)) %>%
    arrange(group, x) %>%
    group_by(group) %>%
    summarise(text = paste(text, collapse = " ")) %>%
    ungroup() %>%
    separate(text, into = c("value", "label"), sep = " ", convert = TRUE, extra = "merge", fill = "right") %>%
    select(label:value) %>%
    mutate(County = county,
           value = parse_number(as.character(value)))
}

# Function for scraping data from Child Care pdfs - Table 4
scrape_services_to_child_care_programs <- function(URL, PAGE = 2) {
  tabulizer::extract_tables(file = URL, pages = PAGE, area = list(c(535, 250, 650, 610))) %>%  # 530, 250, 630, 610
    data.frame() %>%
    mutate_all(str_replace_all,"\r", " ") %>%
    mutate_all(str_replace, "^$", NA_character_) %>%
    janitor::remove_empty("rows") %>%
    janitor::row_to_names(1) %>%
    rename(Location = 1) %>%
    mutate(County = county)
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



# READ MEDIAN INCOME ------------------------------------------------------

county_median_income <- read_csv("data/RAW/ACS/medIncome5yr_acs.csv")


# Calculate median income families with children under 18 years
county_median_family_income <-
  county_median_income %>%
  filter(variable %in% c("B19126_003", "B19126_007", "B19126_010")) %>%
  group_by(fips = GEOID, county_name = NAME, year) %>%
  summarise(median_family_income_with_children_under_18 = mean(estimate)) %>%
  ungroup() %>%
  mutate(county_name = ifelse(fips == 19, "Statewide", county_name)) %>%
  left_join(county_median_income %>%
              filter(variable == "B19126_001")  %>%
              select(fips = GEOID, year, median_family_income = estimate),
            by = c("fips", "year"))



# GET URL FOR PDF FILES ---------------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://iowaccrr.org/data/2020-county-data-sheets/"

# Extracting links of pdf files with child care data
links <- xml2::read_html(URL) %>%
  rvest::html_nodes(".container p a") 

# Extract labels of hyperlinks
links_text <-
  links %>% 
  rvest::html_text() %>%
  str_squish()

# Select links for county-level reports 
county_links <-
  links %>%
  rvest::html_attr("href") %>%
  url_absolute(., URL) %>%
  bind_cols(links_text) %>%
  set_names("href", "text") %>%
  filter(str_detect(href, "pdf$")) %>%
  mutate(county_CAP = standardize_string(text)) %>%
  filter(county_CAP %in% ia_counties$county_CAP)



# SCRAPE CHILD CARE DATA --------------------------------------------------

# Create Tables for Storing Data
child_care_rates <- list()
child_care_counts <- list()
program_spaces_percent <- list()
services_to_child_care_programs <- list()


# Scrape Data from Files 
for (i in 1:nrow(county_links)) {
  URL <- county_links$href[i]
  county <- county_links$text[i]
  print(county)
  child_care_rates[[i]] <- scrape_child_care_rates(URL = URL)
  child_care_counts[[i]] <- scrape_child_care_counts(URL = URL)
  program_spaces_percent[[i]] <- scrape_program_spaces_percent(URL = URL)
  services_to_child_care_programs[[i]] <- scrape_services_to_child_care_programs(URL = URL)
}

cc_rates <- bind_rows(child_care_rates)
cc_counts <- bind_rows(child_care_counts)
cc_spaces <- bind_rows(program_spaces_percent)
cc_services <- bind_rows(services_to_child_care_programs)



# STANDARDIZE & SAVE DATA -------------------------------------------------

# Standardize Childcare Cost Data
cc_rates %>%
  # make column names lower cases and replace space with underscore (_)
  janitor::clean_names() %>%
  # assign levels to age categories
  mutate(age = factor(age, 
                      levels = c("Infant (0-12 Months)",
                                 "Toddler (13-23 Months)",
                                 "Two Year Olds",
                                 "Three Year Olds",
                                 "Four & Five Year Olds",
                                 "Before & After School",
                                 "Full Time School-Age")),
         # add year of measurement
         year = 2020) %>%
  # standardize county names 
  standardize_county() %>%
  select(fips, county, year, provider_type = facility_type, everything()) %>%
  write_csv("data/RAW/childcare/childcare_rates.csv")
  

# Standardize Childcare Space Data
cc_spaces %>%
  # define measurement type for each value
  mutate(temp = ifelse(str_detect(label, "Total"), label, NA_character_)) %>%
  fill(temp, .direction = "down") %>%
  mutate(label = ifelse(str_detect(label, "Total"), "Total", label),
         # standardize labels of provider categories
         label = case_when(
           label == "Child Care Home" ~ "Child Care Homes",
           label == "Dept. of Education Operated Preschool" ~ "Dept. of Education Operated Preschools",
           label == "Registered Child Development Home" ~ "Registered Child Development Homes",
           TRUE ~ label),
         # add year of measurement
         year = 2020,
         temp = word(temp, 2)) %>%
  # transform data into wide format
  spread(temp, value) %>%
  # make column names lower cases and replace space with underscore (_)
  janitor::clean_names() %>%
  # standardize county names 
  standardize_county() %>%
  select(fips, county, year, provider_type = label, everything()) %>%
  write_csv("data/RAW/childcare/childcare_program_spaces.csv")


# Standardize Childcare Count Data
cc_counts %>%
  # add year of measurement
  mutate(year = 2020) %>%
  # standardize county names 
  standardize_county() %>%
  # make column names lower cases and replace space with underscore (_)
  janitor::clean_names() %>%
  select(fips, county, year, provider_type = label, everything()) %>%
  write_csv("data/RAW/childcare/childcare_program_spaces_percent.csv")


# Standardize Childcare Count Data
cc_services %>%
  # add year of measurement
  mutate(year = 2020) %>%
  # standardize county names 
  standardize_county() %>%
  # make column names lower cases and replace space with underscore (_)
  janitor::clean_names() %>%
  select(fips, county, year, everything()) %>%
  write_csv("data/RAW/childcare/childcare_program_services.csv")



# CALCULATE INDICATORS ----------------------------------------------------

# Read Childcare Capacity Data
spaces <- read_csv("data/RAW/childcare/childcare_program_spaces.csv") 

# Calculate Indicator 
spaces_indicator <-
  spaces %>%
  filter(provider_type == "Total") %>%
  # add statewide sum
  janitor::adorn_totals("row", fill = "Statewide", name = "19") %>% 
  mutate(fips = as.numeric(fips),
         year = ifelse(year > 2050, year/99, year)) %>%
  # merge the latest (2019) child population data since there is no 2020
  left_join(county_population_under_6 %>%
              filter(year == 2019) %>%
              select(fips, population_under_6)) %>%
  # calculate indicator
  mutate(childcare_availibility_index = population_under_6 / spaces,
         childcare_desert = case_when(
           population_under_6 >= 50 & programs == 0 ~ "Yes",
           population_under_6 >= 50 & childcare_availibility_index >= 3 ~ "Yes",
           TRUE ~ "No")) %>%
  select(fips, county, year, population_under_6,
         number_of_childcare_providers = programs, capacity = spaces, 
         childcare_availibility_index, childcare_desert)

# Save Data
write_rds(spaces_indicator, "data/CLEAN/childcare_space.rds", compress = "xz")


# Read Childcare Cost Data
rates <- read_csv("data/RAW/childcare/childcare_rates.csv") 



# Calculate Indicator
rates_indicator <-
  rates %>%
  # calculate state average cost per category
  group_by(year, provider_type, age) %>%
  summarise(cost = mean(cost, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fips = 19,
         county = "Statewide",
         cost_average_per_week = paste0("$", round(cost, 2))) %>%
  select(names(rates)) %>%
  # combine statewide and county data
  bind_rows(rates) %>%
  # merge the latest (2019) median family income data since there is no 2020
  left_join(county_median_family_income %>%
              filter(year == 2019) %>%
              select(fips, median_family_income, 
                     median_family_income_with_children_under_18)) %>%
  # calculate indicator
  mutate(average_weekly_income_with_children = median_family_income_with_children_under_18/52,
         percent_of_median_family_income = cost/average_weekly_income_with_children) 

# Save Data
write_rds(rates_indicator, "data/CLEAN/childcare_rates.rds", compress = "xz")


