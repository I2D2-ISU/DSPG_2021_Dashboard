library(ggthemes)

# Data --------------------------------------------------------------------

# read the data
data <- readxl::read_xlsx("data/ECI_Needs_Assessment_Data_final_GIO.xlsx") %>%
  arrange(FIPS)

iowa_counties <- data %>%
  filter(FIPS != 19000) %>%
  pull(county)

variables <- readxl::read_xlsx("data/Variables.xlsx") %>%
  mutate(group_3 = factor(group_3,
                           levels = c("Less than High School Graduate",
                                      "High School Graduate",
                                      "Some College or Associate's Degree",
                                      "Bachelor's Degree",
                                      "Graduate or Professional Degree")))

data_state_wide <- data %>% filter(FIPS == 19000)

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
k_assessment <-
  read_rds("data/CLEAN/k_assessment.rds") %>%
  janitor::clean_names() 

data_k_assessment <-
  k_assessment %>%
  group_by(year) %>%
  summarise(percent_met_benchmark = sum(number_met_benchmark, na.rm = TRUE)/
              sum(number_tested, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(county = "Statewide", fips = 19) %>%
  bind_rows(k_assessment) %>%
  select(names(k_assessment))


