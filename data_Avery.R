library(readr)

acs_inds <- read_csv("data/ACS_fiveYearIndicators.csv")

immun <- read_csv("data/two_yr_old_immunization_rate.csv") %>% select(-X1, NAME=County, year=Year) %>%
  mutate(Percent=Percent/100)

immun_state <- immun %>%
  group_by(year) %>%
  summarise(Percent=mean(Percent, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

immun_clean <- rbind(immun, immun_state)

ser_crime_raw <- read_csv("data/serious_crime_rate_2019_geography.csv") %>% select(NAME=County, year=Year, per100kRate)

ser_crime_state <- ser_crime_raw %>%
  group_by(year) %>%
  summarise(per100kRate=sum(per100kRate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

ser_crime <- rbind(ser_crime_raw, ser_crime_state)

vlow_bw_raw<- read_csv("data/very_low_birthweight.csv") %>% janitor::clean_names() %>%
  select(NAME=county, rate=very_lbw_births_percent, year) %>%
  mutate(rate=rate/100,
         year=2019)

vlow_bw_state <- vlow_bw_raw %>%
  group_by(year) %>%
  summarise(rate=mean(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

vlow_bw_clean <- rbind(vlow_bw_raw, vlow_bw_state)

teen_moms_raw <- read_csv("data/mothers_under_20_birth_rate_per_1k.csv") %>% janitor::clean_names() %>%
  select(NAME=county, year, rate=rate_per_1k_women_15_19)

teen_moms_state <- teen_moms_raw %>%
  group_by(year) %>%
  summarise(rate=sum(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

teen_moms <- rbind(teen_moms_raw,teen_moms_state)

juv_crime_raw <- read_csv("data/juvenile_arrest_rate_2019_statewide.csv") %>% select(NAME = reporting_Counties, year, rate = per_100k_Rate) %>%
  filter(NAME !="Statewide")

juv_crime_state <-juv_crime_raw %>%
  group_by(year) %>%
  summarise(rate=sum(rate, na.rm=TRUE)) %>%
  mutate(NAME="Statewide")

juv_crime <- rbind(juv_crime_raw, juv_crime_state)


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

