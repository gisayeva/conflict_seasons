#R script for merging several datasets
setwd("/Users/galinaisayeva/RA/Seasonality_of_conflict/merge_data")
library(tidyverse)
library(expss)
library(haven)

states <- read.csv("state_system/system2016.csv")
states <- states %>% mutate(majpow = case_when(
  ccode==2 & year>=1898 ~ 1,
  ccode==200 & year>=1816 ~1,
  (ccode==220 & year>=1816 & year <=1940) | (ccode==220 & year>=1945) ~1,
  (ccode==255 & year <=1918) | (ccode==255 & year>=1925 & year <= 1945) | (ccode==255 & year>=1991) ~1,
  ccode==300 & year<=1918 ~1,
  ccode==325 & year>=1860 & year <=1943 ~1,
  (ccode==365 & year<= 1917) | (ccode == 365 & year >=1922) ~1, 
  ccode==710 & year>=1950 ~1,
  (ccode==740 & year>=1895 & year <= 1945) |(ccode==740 & year>=1991) ~1,
  TRUE ~0,
)) %>%
  #generate system information by year
  group_by(year) %>%
  mutate(numGPs = sum(majpow), numstates = n_distinct(ccode)) %>%
  apply_labels( majpow ="Major power - 1 if state is a major power in a given year, 0 otherwise",
                numGPs = "Number of great powers in the system",
                numstates= "Number of states in the system") %>%
  select(-c(version))

#get capabilities data
capab <- read_dta("NMC_6.0/NMC-60-abridged/NMC-60-abridged.dta") %>%
  select(-c(version, stateabb)) %>%
  rename(cap= cinc)

#merge major power data with capabilities data
states <- states %>% left_join(capab, by= c("ccode", "year"))

#make a duplicate to differentiate between state 1 and state 2
states_1 <- states
names(states_1) <- paste0(names(states), "_1")
states_1 <- states_1 %>% rename(year = year_1,numGPs = numGPs_1, numstates= numstates_1 )

#make duplicate for state 2
states_2 <- states
names(states_2) <- paste0(names(states), "_2")
states_2 <- states_2 %>% rename(year = year_2,numGPs = numGPs_2, numstates= numstates_2)

#here, import dataset from MIDs that you use
keys <- read_dta("dyadic_mid_4.01.dta") %>%
  select(statea, stateb, year) %>%
  unique() %>%
  rename(ccode_1 = statea, ccode_2 = stateb)

directed_dyad_18162010 <- left_join(keys, states_1, by = c("ccode_1", "year")) %>% left_join(states_2, by= c("ccode_2", "year")) 

#clean contiguity data
contg<- read_dta("DirectContiguity320/contdird.dta") %>%
  rename(ccode_1 = state1no, ccode_2 = state2no) %>%
  select(-c(state1ab, state2ab, version)) %>%
  group_by(ccode_1, ccode_2, year) %>%
  slice_max(order_by = "conttype") %>%
  ungroup()

#merge on contiguity data
directed_dyad_18162010 <- directed_dyad_18162010 %>% left_join(contg, by = c("ccode_1", "ccode_2", "year")) %>% mutate(conttype = replace_na(conttype,0))