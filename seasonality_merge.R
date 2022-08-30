#R script for merging several datasets
setwd("/Users/galinaisayeva/RA/Seasonality_of_conflict/merge_data")
library(tidyverse)
library(expss)

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
  apply_labels( majpow ="Major power - 1 if state is a major power in a given year, 0 otherwise") 
           