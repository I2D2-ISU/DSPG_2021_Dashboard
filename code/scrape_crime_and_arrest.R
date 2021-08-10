# SCRAPE SERIOUS CRIME DATA -----------------------------------------------
# This script scrapes html table of Crime date from FBI website
# URL: https://ucr.fbi.gov/crime-in-the-u.s
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

