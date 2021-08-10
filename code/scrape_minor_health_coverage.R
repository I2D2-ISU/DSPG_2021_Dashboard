# GET HEALTH COVERAGE DATA ------------------------------------------------
# This script downloads health coverage data from Census SAHIE using API
# URL: https://www.census.gov/data-tools/demo/sahie/#/
# File is cleaned and indicators calculated

library(tidyverse)
library(censusapi)



# READ AUXILIARY DATA -----------------------------------------------------

# Read county fips codes
ia_counties <- 
  read_csv("data/RAW/ACS/pop5yr_acs.csv") %>%
  distinct(fips = GEOID,
           county_name = NAME) %>%
  mutate(county_CAP = standardize_string(county_name))



# DOWNLOAD HEALTH COVERAGE DATE -------------------------------------------

data <- 
  getCensus(name = "timeseries/healthins/sahie", 
            vars = c("NAME", "GEOID", "YEAR", "AGE_DESC", "IPRCAT", "IPR_DESC", 
                     # total population, number and percentages of insured and uninsured
                     "NIPR_PT", "NIC_PT", "NUI_PT", "PCTIC_PT", "PCTUI_PT"), 
            # select all counties
            region = "county:*", 
            # select Iowa
            regionin = "state:19", 
            # select all and under 200% poverty
            IPRCAT = "0,1",
            # select minors only (under 19)
            AGECAT = 4)


data_statewide <- 
  getCensus(name = "timeseries/healthins/sahie", 
            vars = c("NAME", "GEOID", "YEAR", "AGE_DESC", "IPRCAT", "IPR_DESC", 
                     # total population, number and percentages of insured and uninsured
                     "NIPR_PT", "NIC_PT", "NUI_PT", "PCTIC_PT", "PCTUI_PT"), 
            # select statewide data for Iowa
            region = "state:19",  
            # select all and under 200% poverty
            IPRCAT = "0,1",
            # select minors only (under 19)
            AGECAT = 4)


# STANDARDIZE DATA --------------------------------------------------------

minor_health_coverage <-
  bind_rows(data_statewide, data) %>%
  mutate(fips = ifelse(GEOID == 19000, 19, GEOID),
         fips = as.numeric(fips)) %>%
  as.tibble() %>%
  left_join(ia_counties) %>%
  select(fips, county = county_name, year = YEAR, 
         age = AGE_DESC, income = IPR_DESC,
         total = NIPR_PT,
         insured = NIC_PT,
         uninsured = NUI_PT,
         insured_percent = PCTIC_PT,
         uninsured_percent = PCTUI_PT) %>%
  mutate(county = ifelse(fips == 19, "Statewide", county)) %>%
  mutate(across(ends_with("percent"), ~ .x / 100))
  

# Save Data
write_rds(minor_health_coverage, "data/CLEAN/minor_health_coverage.rds", compress = "xz")

