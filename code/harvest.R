# K.Palof   7-6-2020
# exploration of harvest data over time in prep for discontinuation of survey for red king crab

# Objective: explore the patterns of harvest regionwide in SE Alaska RKC fishery
#   compare harvest levels to regional biomass levels eventually


# Load -------------
source('./code/helper.R')

# Data ---------------
cur_yr <- 2019

harvest1 <- read_excel("./data/RKC Harvest for Katie 78.79-96.97.xlsx", sheet = 1)
harvest2 <- read_excel("./data/RKC Harvest for Katie 97.98-17.18.xlsx", sheet = 1)
fishery.status <- read.csv('C:/Users/kjpalof/Documents/SE_crab_assessments/data/rkc/Juneau/hind_fore_cast_JNU_current.csv')

## commercial fishery status -----------------
fishery.status %>% 
  select(Year = year, status) %>% 
  mutate(status = ifelse(status == "PU only", "closed", as.character(status))) %>% #-> fishery.status.update
  # add next line to deal with current year which is TBD in file but will most 
  # likely be closed in current year (2018)
  mutate(status = ifelse(status == "TBD", "closed", as.character(status))) %>% 
  select(year = Year, status) -> fishery.status.update


# data clean-up ------
harvest1 %>% 
  as.data.frame() %>% 
  select(year = YEAR, tkt_no = TICKET_NO, cfec = CFEC_NO, catch_date = CATCH_DATE, 
         )