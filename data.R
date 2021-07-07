read_ACS <- function(file_path) {
  read_csv(file_path) %>%
    select(fips = GEOID, county = NAME, variable, estimate, year) %>%
    spread(variable, estimate) %>%
    mutate(county = ifelse(fips == "19", "Statewide", county))
} 

data_ACS <- 
  read_ACS("data/ACS/newMomEducation5yr_acs.csv") %>%
  mutate(var004 = B13014_004/B13014_002,
         var005 = B13014_005/B13014_002,
         var006 = B13014_006/B13014_002,
         var007 = B13014_007/B13014_002,
         var008 = B13014_008/B13014_002,
         var010 = B13014_010/B13014_002,
         var011 = B13014_011/B13014_002,
         var012 = B13014_012/B13014_002,
         var013 = B13014_013/B13014_002,
         var014 = B13014_014/B13014_002)

