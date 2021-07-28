library(readr)

acs_inds <- read_csv("data/ACS_fiveYearIndicators.csv")

ind_pup6 <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B17020_pup6)

ind_pil <-  acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B23008_pil)