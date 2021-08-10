# SCRAPE K ASSESSMENT DATA ------------------------------------------------
# This script scrapes Kindergarten Assessment data from pdf files from ECI
# URL: https://earlychildhood.iowa.gov/document/kindergarten-assessment-tables
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



# READ COUNTY POPULATION --------------------------------------------------

county_population <- read_csv("data/RAW/ACS/pop5yr_acs.csv")


# Create standardize IA County list
ia_counties <- 
  county_population %>%
  distinct(fips = GEOID, county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# GET URL FOR PDF FILES ---------------------------------------------------

# URL of the webpage to scrape data from
URL  <- "https://earlychildhood.iowa.gov/document/kindergarten-assessment-tables"

# Extracting links of excel files with child abuse data
xml2::read_html(URL) %>%
  rvest::html_nodes(xpath = '//*[@id="block-early-childhood-content"]/article/div[3]') %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href") %>%
  url_absolute(., URL) %>%
  str_subset("pdf$")-> pdf_file_links



# SCRAPE CHILD CARE DATA --------------------------------------------------

# Function for scraping tables from K assessment
scrape_k_assessment <- function(URL, PAGE = 1, METHOD = "decide") {
  tabulizer::extract_tables(file = URL, pages = PAGE, method = METHOD) %>%
    data.frame() %>% 
    tibble() %>%
    mutate(across(everything(), ~ str_replace(.x, "^$", NA_character_))) %>%
    janitor::remove_empty('cols') %>%
    mutate_all(as.character) %>%
    set_names(c(paste("var", 1:ncol(.), sep = "_")))
}


# Function to scrape 2015-2016 data
scrape_old_assessment_tables <- function(URL) {
  my_year <- 2000 + basename(URL) %>% str_extract("\\d{2}") %>% as.numeric()
  
  my_data <-
    map_df(1:2, scrape_k_assessment, URL = URL, METHOD = "decide") %>%
    set_names("ECI_Number", "ECI_Area", 
              "Number_Met_Benchmark", "Number_Tested", "Percent_Met_Benchmark",
              "Test", "Buildings_Reporting") %>%
    slice(-1) %>%
    mutate(across(3:5, ~ parse_number(.))) %>%
    mutate(Percent_Met_Benchmark = Percent_Met_Benchmark/100,
           Year = my_year)
  
  return(my_data)
}


# Function to scrape data from 2017
scrape_new_assessment_tables <- function(URL) {
  
  my_year <- basename(URL) %>% str_extract("\\d{4}") %>% as.numeric()

  pages <- tabulizer::get_n_pages(URL)
  
  my_data <- 
    map_dfr(1:pages, 
            ~ tabulizer::extract_tables(URL, pages = .x, output = "data.frame", method = "stream") %>%
              # tabulizer::extract_tables(example17, pages = 4, output = "data.frame", method = "stream") %>%
              pluck(1) %>%
              # data.frame() %>%
              tibble() %>%
              janitor::remove_empty("cols") %>%
              set_names(paste0("X", 1:ncol(.))) %>%
              # handle problematic pages (page 4) in 2017
              {if (ncol(.) == 6 & sum(str_detect(.$X6, "Benchmark")) == 1) {
                mutate(.data = ., 
                       X1 = paste0(X1, X2),
                       X2 = NULL)
                # handle problematic pages in 2020
              } else if (ncol(.) == 6 & sum(str_detect(.$X6, "Benchmark")) == 0) {
                mutate(.data = ., 
                       X5 = paste0(X5, X6),
                       X6 = NULL)
              } else {
                .
              }} %>%
              set_names(paste0("X", 1:ncol(.))) %>%
              mutate(X11 = ifelse(str_detect(X1, "[:alpha:]"), X1, NA),
                     X11 = str_remove(X11, "^\\d{1,2}\\-")) %>%
              fill(X11) %>%
              filter(str_detect(X1, "^\\d{2}$")) %>%
              mutate(Percent_Met_Benchmark = parse_number(X5)/100,
                     Comments = ifelse(is.na(Percent_Met_Benchmark), X5, "")) %>%
              select(ECI_Area = X11, 
                     AEA = X1, 
                     District = X2, 
                     Number_Met_Benchmark = X3, 
                     Number_Tested = X4, 
                     Percent_Met_Benchmark,
                     Comments)
            )

  my_data <- my_data %>% mutate(Year = my_year)

  return(my_data)
}



# Scrape and combine 2015-2016 data
k_2015_2016 <- map_dfr(pdf_file_links[5:6], scrape_old_assessment_tables)

# Scrape and combine 2017-2020 data
k_2017_2020 <- map_dfr(pdf_file_links[1:4], scrape_new_assessment_tables)



# STANDARDIZE & SAVE DATA -------------------------------------------------

# Read List of ECI Areas and Corresponding Counties
eci_areas <- read_csv("data/ECI_Area_Codes.csv")

k_2015_2016_standard <- 
  k_2015_2016 %>%
  mutate(ECI_Number = as.numeric(ECI_Number),
         Test = word(Test, 1)) %>%
  left_join(eci_areas %>% distinct(ECI_Number, ECI_Area_Name)) %>%
  select(ECI_Number, ECI_Area_Name, Year, Test, Number_Met_Benchmark:Percent_Met_Benchmark) 

# Select on county from each ECI area and create a list to match
key_counties <- 
  eci_areas %>%
  filter(key == "County3") %>%
  pull(county) %>%
  paste(collapse = "|")

k_assessment <- 
  k_2017_2020 %>%
  mutate(county = str_extract(ECI_Area, key_counties),
         Test = "FAST",
         Number_Met_Benchmark = parse_number(Number_Met_Benchmark),
         Number_Tested = parse_number(Number_Tested)) %>%
  left_join(eci_areas, by = "county") %>% 
  select(ECI_Number, ECI_Area_Name, District, Year, Test, Number_Met_Benchmark:Percent_Met_Benchmark, Comments) %>% 
  bind_rows(k_2015_2016_standard) %>%
  arrange(Year, ECI_Number)

write_csv(k_assessment, "data/RAW/K_assessment/K_assessment_data.csv")




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


