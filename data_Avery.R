library(readr)

acs_inds <- read_csv("data/ACS_fiveYearIndicators.csv")

ind_pup6 <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B17020_pup6)

ind_pil <-  acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B23008_pil)

ind_plesshigh <- acs_inds %>% filter(year==max(year), NAME=="Statewide") %>%
  pull(B13014_plesshigh)


trial <- acs_inds %>%
  select(name = NAME, year, all_in_LF = B23008_pil, no_in_LF = B23008_pnl)
