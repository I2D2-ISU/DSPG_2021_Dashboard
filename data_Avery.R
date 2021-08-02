library(readr)

acs_inds <- read_csv("data/ACS_fiveYearIndicators.csv")

immun <- read_csv("data/two_yr_old_immunization_rate.csv") %>% select(-X1, NAME=County, year=Year) %>%
  mutate(Percent=Percent/100)

immun_state <- immun %>%
  group_by(year) %>%
  summarise(Percent=mean(Percent, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

immun_clean <- rbind(immun, immun_state)

ind_pup6 <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B17020_pup6)

ind_pil <-  acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B23008_pil)

ind_plesshigh <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B13014_plesshigh)

ind_immun <- immun_clean %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(Percent)

