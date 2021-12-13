library(ggthemes)

# Data --------------------------------------------------------------------

# read the data
data <- readxl::read_xlsx("data/ECI_Needs_Assessment_Data_final_GIO.xlsx") %>%
  distinct(fips = FIPS, county) %>%
  mutate(county = ifelse(fips == 19000, "Statewide", county))

iowa_counties <- data %>%
  filter(fips != 19000) %>%
  pull(county)

variables <- readxl::read_xlsx("data/Variables.xlsx") %>%
  mutate(group_3 = factor(group_3,
                           levels = c("Less than High School Graduate",
                                      "High School Graduate",
                                      "Some College or Associate's Degree",
                                      "Bachelor's Degree",
                                      "Graduate or Professional Degree")))

# data_state_wide <- data %>% filter(FIPS == 19000)

varnames <- readxl::read_xlsx("data/ECI_Needs_Assessment_Data_final_GIO.xlsx", 
                              sheet = 2, range = readxl::cell_cols("A:B"))

iowa_map <- sf::st_as_sf(map(database = "county", 
                             regions = "iowa", 
                             fill = TRUE, 
                             plot = FALSE)) %>%
  # add county fips code
  left_join(county.fips, by = c("ID" = "polyname")) %>%
  separate(ID, into = c("state", "county"), sep = ",", remove = FALSE)



# Read row ACS data

read_ACS <- function(file_path) {
  read_csv(file_path) %>%
    select(fips = GEOID, county = NAME, variable, estimate, year) %>%
    spread(variable, estimate) %>%
    mutate(county = ifelse(fips == "19", "Statewide", county))
} 

data_ACS <- 
  read_ACS("data/RAW/ACS/newMomEducation5yr_acs.csv") %>%
  mutate(var004 = B13014_004/B13014_002,
         var005 = B13014_005/B13014_002,
         var006 = B13014_006/B13014_002,
         var007 = B13014_007/B13014_002,
         var008 = B13014_008/B13014_002,
         var010 = B13014_010/B13014_002,
         var011 = B13014_011/B13014_002,
         var012 = B13014_012/B13014_002,
         var013 = B13014_013/B13014_002,
         var014 = B13014_014/B13014_002) %>%
  select(fips:year, starts_with("var")) %>%
  gather(var, value, starts_with("var")) %>%
  left_join(variables, by = "var") %>%
  select(fips, county, year, group_2, group_3, value) %>%
  spread(group_2, value) %>% 
  mutate(Both = Married + Unmarried) %>%
  gather(group_2, value, Married:Both)

# Read K assessment data
data_k_assessment <- read_csv("data/k_assessment.csv")




# Sonyta's Data -----------------------------------------------------------

child_abuse <- read_csv("data/child_abuse/child_abuse.csv") %>% 
  select(-1) %>%
  rename(county = name) %>%
  mutate(child_abuse_under_6 = child_abuse_under_3 + child_abuse_3_and_4 + child_abuse_5)


child_abuse_county_state <-
  child_abuse %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(county = "Statewide") %>%
  bind_rows(child_abuse) %>%
  left_join(data) %>% 
  # add fips for O'Brien (was missing due to misspelling)
  mutate(fips = ifelse(is.na(fips), 19141, fips)) %>%
  select(fips, county, year, 
         age_under_3 = child_abuse_under_3, 
         age_3_and_4 = child_abuse_3_and_4, 
         age_5 = child_abuse_5, 
         age_under_6 =child_abuse_under_6)



childcare_services <- read_csv("data/childcare/childcare_program_services.csv") %>%
  select(-1, -fips)
#childcare_program_services

childcare_spaces_percent <- read_csv("data/childcare/childcare_program_spaces_percent.csv") %>%
  select(-1, -fips)
#childcare_program_spaces_percent

childcare_spaces <- read_csv("data/childcare/childcare_program_spaces_total.csv") %>%
  select(-1)


childcare_rates <- read_csv("data/childcare/childcare_rates.csv") %>%
  select(-1, -`cost_average_per_week`)

childcare_rates_provider1 <- childcare_rates %>%
  filter (provider_type == "Registered Child Development Homes")

childcare_rates_provider2 <- childcare_rates %>%
  filter (provider_type == "DHS Licensed Centers/Preschools")


#unemploy
unemployment_rate_by_year <- 
  read_csv("data/unemploy/unemployment_rate_by_year.csv") %>%
  select(county = name, year, unemprate) %>%
  left_join(distinct(childcare_rates, fips, county), by = "county")

unemploy_state_by_year <- 
  unemployment_rate_by_year %>%
  group_by(year) %>%
  summarise(Percent=mean(unemprate, na.rm=TRUE)) %>%
  mutate(county="Statewide", fips = 19000)

unemployment_rate <- 
  read_csv("data/unemploy/unemployment_rate.csv") %>%
  select(county = name, year, unemprate) %>%
  left_join(distinct(childcare_rates, fips, county), by = "county")

unemploy_state <- 
  unemployment_rate_by_year %>%
  group_by(year) %>%
  summarise(Percent=mean(unemprate, na.rm=TRUE)) %>%
  mutate(county="Statewide", fips = 19000)





# Avery's Data ------------------------------------------------------------

acs_inds <- read_csv("data/ACS_fiveYearIndicators.csv")

immun <- read_csv("data/two_yr_old_immunization_rate.csv") %>% select(-1, NAME=County, year=Year, Percent) %>%
  mutate(Percent=Percent/100)

immun_state <- immun %>%
  group_by(year) %>%
  summarise(Percent=mean(Percent, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

immun_clean <- rbind(immun, immun_state)


ser_crime_raw <- read_csv("data/serious_crime_rate_2019_geography.csv") %>% 
  select(NAME=County, year=Year, per100kRate)

ser_crime_state <- ser_crime_raw %>%
  group_by(year) %>%
  summarise(per100kRate=sum(per100kRate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

ser_crime <- rbind(ser_crime_raw, ser_crime_state) %>%
  left_join(distinct(acs_inds, NAME, fips = GEOID))

vlow_bw_raw<- read_csv("data/very_low_birthweight.csv") %>% janitor::clean_names() %>%
  select(NAME=county, rate=very_lbw_births_percent, year) %>%
  mutate(rate=rate/100,
         year=2019)

vlow_bw_state <- vlow_bw_raw %>%
  group_by(year) %>%
  summarise(rate=mean(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

vlow_bw_clean <- rbind(vlow_bw_raw, vlow_bw_state)

teen_moms_raw <- read_csv("data/mothers_under_20_birth_rate_per_1k.csv") %>% 
  janitor::clean_names() %>%
  select(NAME=county, year, rate=rate_per_1k_women_15_19)

teen_moms_state <- teen_moms_raw %>%
  group_by(year) %>%
  summarise(rate=sum(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

teen_moms <- rbind(teen_moms_raw,teen_moms_state)

juv_crime_raw <- read_csv("data/juvenile_arrest_rate_2019_statewide.csv") %>%
  select(NAME = reporting_Counties, year, rate = per_100k_Rate) %>%
  filter(NAME !="Statewide")

juv_crime_state <-juv_crime_raw %>%
  group_by(year) %>%
  summarise(rate=sum(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

juv_crime <- rbind(juv_crime_raw, juv_crime_state) %>%
  left_join(distinct(acs_inds, NAME, fips = GEOID))


# --- Numbers for Indicator Page

ind_pup6 <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B17020_pup6)

ind_pil <-  acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B23008_pil)

ind_plesshigh <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B13014_plesshigh)

ind_immun <- immun_clean %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(Percent)

ind_ser_crime <- round(ser_crime %>% filter(year==max(year), NAME=="Statewide") %>%
                         pull(per100kRate))

ind_teen_moms <- round(teen_moms %>% filter(year==max(year), NAME=="Statewide") %>%
                         pull(rate))

ind_juv_crime <- round(juv_crime %>% filter(year==max(year), NAME=="Statewide") %>%
                         pull(rate))

ind_vlow_bw <- vlow_bw_clean %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(rate)


