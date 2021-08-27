# SCRAPE SERIOUS CRIME DATA -----------------------------------------------
# This script scrapes html table of Crime date from FBI website
# URL: https://ucr.fbi.gov/crime-in-the-u.s
# File is cleaned and indicators calculated

# https://ucr.fbi.gov/nibrs/2011/tables/data-tables

# # NIBRS
# website: https://rpubs.com/johnbradford/interracial-violence
# https://s3-us-gov-west-1.amazonaws.com/cg-d3f0433b-a53e-4934-8b94-c678aa2cbaf3/2000/IA-2000.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d3f0433b-a53e-4934-8b94-c678aa2cbaf3/2005/IA-2005.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d3f0433b-a53e-4934-8b94-c678aa2cbaf3/2009/IA-2009.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d3f0433b-a53e-4934-8b94-c678aa2cbaf3/2013/IA-2013.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d3f0433b-a53e-4934-8b94-c678aa2cbaf3/2015/IA-2015.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/2016/IA-2016.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/2018/IA-2018.zip
# https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/2019/IA-2019.zip


# API
# Tutorial: https://youtu.be/lV3u6Dc93T0
# FBI API: https://crime-data-explorer.fr.cloud.gov/pages/docApi
# FBI package: https://github.com/jacobkap/fbi/blob/master/R/ucr_arrest.R
# Other example about data.gov: https://data.library.virginia.edu/using-data-gov-apis-in-r/


a <- "https://api.usa.gov/crime/fbi/sapi/"
k <- "?API_KEY=pqNgV7sonRNr1qyFZVz6v11tejcRMlwSxZZd5hdM"
# ORI
# Agency level Arrest Demographic Count By Offense Endpoint
b1 <-  "api/data/arrest/agencies/IA0010000/all-other-offenses/offense/2018/2019" # variables = male, female, offense, race, monthly
# Agency level Arrest Demographic Count Endpoint
b2 <-  "api/data/arrest/agencies/offense/IA0010000/all/2018/2019" # variables = all, monthly
# STATE
# State level Arrest Demographic Count By Offense Endpoint
b3 <-  "api/data/arrest/states/IA/all-other-offenses/offense/2018/2019" # variables = male, female, offense, race, monthly
# State level Arrest Demographic Count Endpoint
b4 <-  "api/data/arrest/states/offense/IA/all/2018/2019"  # variables = all, monthly
paste0(a,b1,k)
paste0(a,b2,k)
paste0(a,b3,k)
paste0(a,b4,k)

# FOOTNOTES 
"https://api.usa.gov/crime/fbi/sapi/api/footnotes/states/IA/estimated?API_KEY=pqNgV7sonRNr1qyFZVz6v11tejcRMlwSxZZd5hdM"
# PARTICIPATION
"https://api.usa.gov/crime/fbi/sapi/api/participation/states/IA?API_KEY=pqNgV7sonRNr1qyFZVz6v11tejcRMlwSxZZd5hdM"
# PARTICIPATION Downloadable CSV
"https://api.usa.gov/crime/fbi/sapi/api/participation/dl/states/IA?API_KEY=pqNgV7sonRNr1qyFZVz6v11tejcRMlwSxZZd5hdM"
# LOOKUPS - IA UCR Agency Data
"https://api.usa.gov/crime/fbi/sapi/api/agencies/byStateAbbr/IA?API_KEY=pqNgV7sonRNr1qyFZVz6v11tejcRMlwSxZZd5hdM"

library(httr)
# /api/data/arrest/agencies/{ori}/{offense}/{variable}/{since}/{until}

data <- list()
for (i in c("male", "female")) {
  df <- list()
  for (j in iowa_agencies$ori[1:5]) {
    b1 <- paste0("api/data/arrest/agencies/", j, "/all/", i, "/2015/2017")
    response <- GET(paste0(a,b1,k))
    if (response$status_code == 200) {
      response <- jsonlite::fromJSON(rawToChar(response$content))
      response <- response$results
      if (!is_empty(response)) {
        df[[j]] <-
          response %>%
          gather("age", "arrests", starts_with("range")) %>%
          mutate(sex = i,
                 ori = j) %>%
          select(ori, sex, year = data_year, age, arrests) 
      }
    }
  }
  data[[i]] <- bind_rows(df)
}

data

b1 <-  "api/data/arrest/agencies/IA0900400/all/female/1985/2019" # variables = male, female, offense, race, monthly
response <- GET(paste0(a,b1,k))
if (response$status_code == 200) {
  response <- jsonlite::fromJSON(rawToChar(response$content))
  response <- response$results
}
response %>%
  gather("age", "arrests", starts_with("range")) %>%
  mutate(sex = "male",
         ori = "ORI") %>%
  select(ori, sex, year = data_year, age, arrests)







get_arrest_demographics(
  # ori = "IA0010000",
  state_abb = "IA",
  # region = "Midwest",
  offense = "all",
  key = get_api_key())
# DB
# Presentation about FBI dbs: http://washstat.org/presentations/20190923/Thomas_Ian.pdf



library(tidyverse)
library(readxl)
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


# SCRAPE CRIME DATA -------------------------------------------------------

# # Function to extract tables with QRS providers 
# scrape_QRS_provider_list <- function(URL) {
#   scrape_date <- 
#     xml2::read_html(URL) %>%
#     rvest::html_nodes("div.field-items h2") %>%
#     rvest::html_text()
#   
#   df <- 
#     xml2::read_html(URL) %>%
#     rvest::html_node("tbody") %>%
#     rvest::html_table() %>%
#     janitor::row_to_names(1) %>%
#     janitor::clean_names() %>%
#     mutate(month = str_extract(scrape_date, "[:alpha:]+"),
#            year = str_extract(scrape_date, "[:number:]+"))
#   
#   return(df)
# }
# 
# 
# # Read Certified Child Development Homes List
# QRS_provider_home <- scrape_QRS_provider_list("https://dhs.iowa.gov/iqrs/providers/homes")
# 
# 
# # Save Raw data
# write_csv(QRS_provider_list, "data/RAW/QRS/qrs_iowa_monthly.csv")




# READ DATA ---------------------------------------------------------------

# Read Arrest Tables

file_path <- "data/RAW/crime/arrests_statewide/table-69_Arrests-by-State_1999.xls"

df <- read_xls(file_path, range = cell_cols(1), col_names = FALSE) 
first_row <- which(df[1] == 'State')
col_names <-
  read_xls(file_path, range = cell_rows(first_row), col_names = FALSE) %>%
  mutate_all(function(x) {x %>% 
      str_remove_all("[:number:]") %>%
      str_remove_all("-\\s*") %>%
      str_squish() %>%
      str_to_lower()
    })  %>% 
  unlist(., use.names = FALSE) %>%
  str_replace("^state$", "age") %>%
  janitor::make_clean_names()

df <-
  read_xls(file_path, skip = first_row, col_names = col_names,
           range = cell_cols(1:length(col_names))) %>%
  mutate(state = str_extract(age, "[:upper:]{3,}")) %>%
  fill(state, .direction = "down") %>%
  filter(state == "IOWA") 

population <- 
  df %>% 
  filter(is.na(total_all_classes)) %>%
  janitor::remove_empty("cols") %>%
  mutate(value = parse_number(age),
         variable = str_remove(age, state) %>%
           str_remove_all("[:punct:]") %>%
           str_remove_all("[0-9]*") %>%
           str_squish()) %>%
  select(value, variable) %>%
  spread(variable, value)

data <- filter(df, !is.na(total_all_classes)) %>%
  mutate(state = str_to_sentence(state),
         year = str_extract(basename(file_path), "\\d{4}"))  %>% 
  bind_cols(population) %>%
  select(state, year, 
         estimated_population = population, 
         number_of_agencies  = agencies, 
         age, everything())

meta <-
  read_xls(file_path, range = cell_rows(first_row), col_name = col_names) %>%
  gather(variable, value) %>%
  filter(str_detect(value, "\\d")) %>%
  mutate(footnote_number = str_extract(value, "[:number:]"))

footnote_numbers <- paste0("^[1-", max(meta$footnote_number), "] ")

footnotes <-
  read_xls(file_path, range = cell_cols(1), col_name = "footnote") %>%
  filter(str_detect(footnote, footnote_numbers)) %>%
  mutate(footnote_number = str_extract(footnote, "[:number:]")) %>%
  right_join(meta) %>%
  select(variable, footnote)
  


# read from 2005 forward

file_path <- "data/RAW/crime/arrests_statewide/table-69_Arrests-by-State_2005.xls"

df <- read_xls(file_path, range = cell_cols(1), col_names = FALSE) 

first_row <- which(df[1] == 'State')
col_names <-
  read_xls(file_path, range = cell_rows(first_row), col_names = FALSE) %>%
  mutate_all(function(x) {x %>% 
      str_remove_all("[:number:]") %>%
      str_remove_all("-\\s*") %>%
      str_squish() %>%
      str_to_lower()
  })  %>% 
  unlist(., use.names = FALSE) %>%
  janitor::make_clean_names() %>%
  str_replace("^na$", "age")

df <-
  read_xls(file_path, skip = first_row, col_names = col_names, 
           range = cell_cols(1:length(col_names))) %>%
  # mutate(state = str_extract(age, "[:upper:]{3,}")) %>%
  fill(state, .direction = "down") %>%
  filter(state == "IOWA") 

# population <- 
#   df %>% 
#   filter(is.na(total_all_classes)) %>%
#   janitor::remove_empty("cols") %>%
#   mutate(value = parse_number(age),
#          variable = str_remove(age, state) %>%
#            str_remove_all("[:punct:]") %>%
#            str_remove_all("[0-9]*") %>%
#            str_squish()) %>%
#   select(value, variable) %>%
#   spread(variable, value)

data <- 
  df %>%
  # filter(!is.na(total_all_classes)) %>%
  mutate(state = str_to_sentence(state),
         year = str_extract(basename(file_path), "\\d{4}"))  %>% 
  # bind_cols(population) %>%
  select(state, year, estimated_population, number_of_agencies, age, everything()) %>%
  fill(estimated_population, number_of_agencies)

meta <-
  read_xls(file_path, range = cell_rows(first_row), col_name = col_names) %>%
  gather(variable, value) %>%
  filter(str_detect(value, "\\d")) %>%
  mutate(footnote_number = str_extract(value, "[:number:]"))

footnote_numbers <- paste0("^[1-", max(meta$footnote_number), "] ")

footnotes <-
  read_xls(file_path, range = cell_cols(1), col_name = "footnote") %>%
  filter(str_detect(footnote, footnote_numbers)) %>%
  mutate(footnote_number = str_extract(footnote, "[:number:]")) %>%
  right_join(meta) %>%
  select(variable, footnote)




# CALCULATE INDICATORS ----------------------------------------------------



















library(XML)
library(readxl)
library(httr)
library(openxlsx)
#install.packages("rvest")
library("rvest")
library(httr)
library(dplyr)
library(tidycensus)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(stringr)

county_map <- as_tibble(map_data("county")) %>%
  filter(region == "iowa") %>%
  mutate(County = str_to_title(subregion))

url_serious_crime_2019 <- GET("https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-10/table-10-state-cuts/iowa.xls")
table_serious_crime_2019 <- html_nodes(content(url_serious_crime_2019), "table")
serious_crime_2019 <- html_table(table_serious_crime_2019[[1]],fill=TRUE)

serious_crime_2019 <- data.frame(serious_crime_2019) %>%
  select(-Metropolitan.Nonmetropolitan)

population_totals <- get_acs(geography = "county",
                             state = "IA",
                             table = "B01003")
population_totals <- population_totals %>%
  select(-moe, -GEOID, -variable) %>%
  separate(NAME, into = c("COUNTY", "STATE"), sep = " County") %>%
  rename("Population" = estimate) %>%
  select(-STATE)

serious_crime_2019_pt <- serious_crime_2019 %>%
  mutate(Total = Violentcrime + Murder.andnonnegligentmanslaughter + Rape1 + Robbery + Aggravatedassault + Propertycrime + Burglary + Larceny.theft + Motorvehicletheft + Arson) %>%
  left_join(population_totals, by = c("County" = "COUNTY"))

serious_crime_2019_geography <- serious_crime_2019_pt %>%
  mutate(per100kRate = 100000*Total/Population) %>%
  mutate(Year = 2019)

serious_crime_2019_map <- ggplot(serious_crime_2019_geography) +
  geom_polygon(data = serious_crime_2019$geometry, aes(x = long, y = lat, group = group, fill = per100kRate), color = "black") +
  labs(title = "Serious Crimes per 100,000 People", fill = "#") +
  theme_void() +
  scale_fill_gradient(low = "white", high = "darkblue")
serious_crime_2019_map


write.csv(serious_crime_2019_geography, file = "clean_data/serious_crime_rate_2019_geography.csv")
#save(serious_crime_2019, file = "personal_folders/Dylan/serious_crime.csv")   NOT WORKING




url_juvenile_arrests <- GET("https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-69")
table_juvenile_arrests <- html_nodes(content(url_juvenile_arrests), "table")
juvenile_arrests_2019 <- html_table(table_juvenile_arrests[[1]], fill = TRUE)

juvenile_arrests_2019 <- data.frame(juvenile_arrests_2019)

juvenile_arrests_2019 <- juvenile_arrests_2019 %>%
  filter(State == "IOWA") %>%
  mutate(Totalall.classes1 = str_remove(Totalall.classes1, ",")) %>%
  mutate(X2019estimated.population = str_remove_all(X2019estimated.population, ",")) %>%
  mutate(Totalall.classes1 = as.numeric(Totalall.classes1), X2019estimated.population = as.numeric(X2019estimated.population)) %>%
  mutate(per100kRate = 100000*Totalall.classes1/X2019estimated.population)





juvenile_arrests_2015 <- read.csv("~/Documents/GitHub/i2d2/personal_folders/Dylan/juvenile_court_case_counts_2015.csv")
juvenile_arrests_2015 <- data.frame(juvenile_arrests_2015)
juvenile_arrests_2015 <- juvenile_arrests_2015 %>%
  mutate(Year = 2015) %>%
  rename("total_Population_Estimate" = Total.2015.Population.Estimates, "10_Through_Upper_Age_Population_Estimate" = X10.Through.Upper.Age.2015.Population.Estimates,
         "0_Through_Upper_Age_Population_Estimate" = X0.Through.Upper.Age.2015.Population.Estimates)

juvenile_arrests_2016 <- read.csv("~/Documents/GitHub/i2d2/personal_folders/Dylan/juvenile_court_case_counts_2016.csv")
juvenile_arrests_2016 <- data.frame(juvenile_arrests_2016)
juvenile_arrests_2016 <- juvenile_arrests_2016 %>%
  mutate(Year = 2016) %>%
  rename("total_Population_Estimate" = Total.2016.Population.Estimates, "10_Through_Upper_Age_Population_Estimate" = X10.Through.Upper.Age.2016.Population.Estimates,
         "0_Through_Upper_Age_Population_Estimate" = X0.Through.Upper.Age.2016.Population.Estimates)

juvenile_arrests_2017 <- read.csv("~/Documents/GitHub/i2d2/personal_folders/Dylan/juvenile_court_case_counts_2017.csv")
juvenile_arrests_2017 <- data.frame(juvenile_arrests_2017)
juvenile_arrests_2017 <- juvenile_arrests_2017 %>%
  mutate(Year = 2017) %>%
  rename("total_Population_Estimate" = Total.2017.Population.Estimates, "10_Through_Upper_Age_Population_Estimate" = X10.Through.Upper.Age.2017.Population.Estimates,
         "0_Through_Upper_Age_Population_Estimate" = X0.Through.Upper.Age.2017.Population.Estimates)

juvenile_arrests_2018 <- read.csv("~/Documents/GitHub/i2d2/personal_folders/Dylan/juvenile_court_case_counts_2018.csv")
juvenile_arrests_2018 <- data.frame(juvenile_arrests_2018)
juvenile_arrests_2018 <- juvenile_arrests_2018 %>%
  mutate(Year = 2018) %>%
  rename("total_Population_Estimate" = Total.2018.Population.Estimate, "10_Through_Upper_Age_Population_Estimate" = X10.Through.Upper.Age.2018.Population.Estimate,
         "0_Through_Upper_Age_Population_Estimate" = X0.Through.Upper.Age.2018.Population.Estimate)

juvenile_arrests <- rbind(juvenile_arrests_2015, juvenile_arrests_2016, juvenile_arrests_2017, juvenile_arrests_2018) %>%
  mutate(Petition = str_remove_all(Petition, ","), Non.petition = str_remove_all(Non.petition, ","), total_Population_Estimate = str_remove_all(total_Population_Estimate, ",")) %>%
  mutate(Petition = as.numeric(Petition), Non.petition = as.numeric(Non.petition), total_Population_Estimate = as.numeric(total_Population_Estimate)) %>%
  mutate(per_100k_Rate = 100000*(Petition + Non.petition)/total_Population_Estimate) %>%
  filter(Reporting.Counties != "Rates for Reporting Counties") %>%
  mutate(Reporting.Counties = str_replace(Reporting.Counties, "Total", "Statewide")) %>%
  rename("reporting_Counties" = Reporting.Counties, "petition" = Petition, "non_Petition" = Non.petition, "year" = Year)







write.csv(juvenile_arrests, file = "personal_folders/Dylan/juvenile_arrest_rate_2015-2018.csv")
write.csv(juvenile_arrests, file = "clean_data/juvenile_arrest_rate_2019_statewide.csv")

