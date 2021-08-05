library(readr)
library(dplyr)

#unemploy
unemployment_rate_by_year <- read_csv("data/unemploy/unemployment_rate_by_year.csv") %>%
  select(name, year, unemprate)

unemploy_state_by_year <- unemployment_rate_by_year %>%          #same
  group_by(year) %>%
  summarise(Percent=mean(unemprate, na.rm=TRUE)) %>%
  mutate(name="Statewide")

unemployment_rate <- read_csv("data/unemploy/unemployment_rate.csv") %>%
  select(name, year, unemprate)

unemploy_state <- unemployment_rate_by_year %>%                  #same
  group_by(year) %>%
  summarise(Percent=mean(unemprate, na.rm=TRUE)) %>%
  mutate(name="Statewide")


child_abuse <- read_csv("data/child_abuse/child_abuse.csv") %>% 
  select(-X1) %>%
  group_by(year) %>%
  mutate(child_abuse_under_6 = child_abuse_under_3 + child_abuse_3_and_4 + child_abuse_5)

#unique(child_abuse$year)

child_abuse_under_3 <- child_abuse %>% 
  summarise(child_abuse_under_3=mean(child_abuse_under_3, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year == unique(year)) %>%
  select(name, year, child_abuse_under_3)

#child_abuse_under_3 

child_abuse_3_and_4 <- child_abuse %>% 
  summarise(child_abuse_3_and_4=mean(child_abuse_3_and_4, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year == unique(year)) %>%
  select(name, year, child_abuse_3_and_4)

child_abuse_5 <- child_abuse %>% 
  summarise(child_abuse_5=mean(child_abuse_5, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year == unique(year)) %>%
  select(name, year, child_abuse_5)



child_abuse_combine <- left_join(left_join(child_abuse_under_3, child_abuse_3_and_4), child_abuse_5) %>%
  mutate(child_abuse_under_6 = child_abuse_under_3 + child_abuse_3_and_4 + child_abuse_5)

child_abuse_county_state <- rbind(child_abuse, child_abuse_combine) 
  
#filter(name =="Statewide")



childcare_services <- read_csv("data/childcare/childcare_program_services.csv") %>%
  select(-X1, -fips)
#childcare_program_services

childcare_spaces_percent <- read_csv("data/childcare/childcare_program_spaces_percent.csv") %>%
  select(-X1, -fips)
#childcare_program_spaces_percent

childcare_spaces <- read_csv("data/childcare/childcare_program_spaces_total.csv") %>%
  select(-X1, -fips)


childcare_rates <- read_csv("data/childcare/childcare_rates.csv") %>%
  select(-X1, -fips, -`cost_average_per_week`)

childcare_rates_provider1 <- childcare_rates %>%
  filter (provider_type == "Registered Child Development Homes")

childcare_rates_provider2 <- childcare_rates %>%
  filter (provider_type == "DHS Licensed Centers/Preschools")





