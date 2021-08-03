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
  group_by(year)

child_abuse_under3 <- child_abuse %>% 
  summarise(abuse=mean(child_abuse_under_3, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year =="2019")

child_abuse_3_and_4 <- child_abuse %>% 
  summarise(abuse=mean(child_abuse_3_and_4, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year =="2019")

child_abuse_5 <- child_abuse %>% 
  summarise(abuse=mean(child_abuse_5, na.rm=TRUE)) %>%
  mutate(name="Statewide") %>%
  filter(year =="2019")

child_abuse_under_6 <- child_abuse_under3$abuse + child_abuse_3_and_4$abuse + child_abuse_5$abuse 


childcare_program_services <- read_csv("data/childcare/childcare_program_services.csv") %>%
  select(-X1, -fips)
childcare_program_services

childcare_program_spaces_percent <- read_csv("data/childcare/childcare_program_spaces_percent.csv") %>%
  select(-X1, -fips)
childcare_program_spaces_percent

childcare_program_spaces_total <- read_csv("data/childcare/childcare_program_spaces_total.csv") %>%
  select(-X1, -fips)
childcare_program_spaces_total

childcare_rates <- read_csv("data/childcare/childcare_rates.csv") %>%
  select(-X1, -fips)
childcare_rates







