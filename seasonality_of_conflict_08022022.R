#replication file for "Seasonality of Conflict"
#begun 08/02/2022
#has updated data as of 07/10/2022
library(tidyverse)
library(haven)
library("expss")
library("npreg")
library(mfp)
library(ggthemes)
library("ggformula")
library("readxl")
setwd("/Users/galinaisayeva/RA/Seasonality_of_conflict/new_data")
#load and clean MID location dataset
temploc <- read_csv("MIDLOC_2.1/MIDLOCA_2.1.csv")
temploc<- rename(temploc, disno = dispnum)
temploc <- select(temploc, -c(year, midloc2_location, midloc2_measuringpoint))

#load and clean mid dataset
mid <- read_dta("dyadic_mid_4.01/dyadic_mid_4.01.dta") %>% 
  select(-c("namea", "nameb")) %>% 
  left_join(temploc, by= c("disno")) %>% 
  arrange(statea, stateb, year) 

#calculate day of start of conflict
mid <- mid %>% mutate(smonthday = case_when(
  strtmnth == 1 ~ 0,
  strtmnth == 2 ~ 31,
  strtmnth == 3 ~ 31+29,
  strtmnth == 4 ~ 31+29+31,
  strtmnth == 5 ~ 31+29+31+30,
  strtmnth == 6 ~ 31+29+31+30+31,
  strtmnth == 7 ~ 31+29+31+30+31+30,
  strtmnth == 8 ~ 31+29+31+30+31+30+31,
  strtmnth == 9 ~ 31+29+31+30+31+30+31+31,
  strtmnth == 10 ~ 31+29+31+30+31+30+31+31+30,
  strtmnth == 11 ~ 31+29+31+30+31+30+31+31+30+31,
  strtmnth == 12 ~ 31+29+31+30+31+30+31+31+30+31+30,
  )) %>% 
  mutate(stdata=strtday+smonthday) %>% 
  apply_labels(stdata = "DATE - day of year state A onset of MID")
 #new from here
mid <- mid %>%  mutate(no1stdata = ifelse(strtday != 1, stdata, NA)) %>%
  apply_labels(no1stdata = "DATE - no first of month, day of year state A onset of MID") %>%
  mutate(no1stdata2=no1stdata*no1stdata) %>%
  select(-c(smonthday))

#calculate day of end of conflict
mid <- mid %>% arrange(endmnth) %>%
  mutate(emonthday = case_when(
    endmnth == 1 ~ 0,
    endmnth == 2 ~ 31,
    endmnth == 3 ~ 31+29,
    endmnth == 4 ~ 31+29+31,
    endmnth == 5 ~ 31+29+31+30,
    endmnth == 6 ~ 31+29+31+30+31,
    endmnth == 7 ~ 31+29+31+30+31+30,
    endmnth == 8 ~ 31+29+31+30+31+30+31,
    endmnth == 9 ~ 31+29+31+30+31+30+31+31,
    endmnth == 10 ~ 31+29+31+30+31+30+31+31+30,
    endmnth == 11 ~ 31+29+31+30+31+30+31+31+30+31,
    endmnth == 12 ~ 31+29+31+30+31+30+31+31+30+31+30,
  )) %>%
  mutate(enddata=endday+emonthday) %>% 
  apply_labels(stdata = "DATE - day of year state A terminate of MID")
  
mid <- mid %>%
  mutate(no1enddata = case_when(
    endmnth==1 & endday==31~ NA_real_,
    endmnth==2 & endday==29~ NA_real_,
    endmnth==3 & endday==31~ NA_real_,
    endmnth==4 & endday==30~ NA_real_,
    endmnth==5 & endday==31~ NA_real_,
    endmnth==6 & endday==30~ NA_real_,
    endmnth==7 & endday==31~ NA_real_,
    endmnth==8 & endday==31~ NA_real_,
    endmnth==9 & endday==30~ NA_real_,
    endmnth==10 & endday==31~ NA_real_,
    endmnth==11 & endday==30~ NA_real_,
    endmnth==12 & endday==31~ NA_real_,
    TRUE~endday+emonthday
  )) %>%
  apply_labels(no1enddata = "DATE - no last of month, day of year state A onset of MID") %>%
  select(-c(emonthday))
  
mid <- mid %>% arrange(disno, statea, stateb, year) %>%
  #drop repeated disputes
  group_by(disno, statea, stateb) %>%
  slice(1)
  #what about when statea= stateb[n-1] disputes? 
####
#in Stata version, 
#this is where months were labelled with 1 "Jan" etc
#months <- c("Jan"=1, 2= "Feb", 3= "Mar",
            #4= "Apr", 5="May", 6="Jun", 7="Jul",
           # 8="Aug", 9="Sep", 10 ="Oct", 11="Nov", 12="Dec")
####

#comment: new data does not have north/south encoded
#create new variable north
mid <- mid %>%  
  mutate(terr = case_when(
  revtypea == 1 | revtypeb ==1 ~ 1,
  TRUE~ NA_real_)) %>%
  mutate(nonterr = case_when(
  revtypea != 1 | revtypeb !=1 ~ 1,
  TRUE~ NA_real_)) %>%
  rename(latitude = midloc2_ylatitude) %>%
  mutate(north = case_when(
    latitude >0  ~ 1,
    TRUE~ 0))

mid <- mid %>%
  arrange(stdata)%>%
  group_by(stdata) %>%
  mutate(stmidperday= sum(north)) %>%
  ungroup()%>%
  mutate(northfat = ifelse(fatlev != "None" & fatlev !="Missing (knon fatalities" & north == 1, 1, 0)) %>%
  mutate(northwar = ifelse(war ==1 & is.na(war)==FALSE&north ==1,1,0)) %>%
  mutate(north19war= ifelse(war ==1 &north ==1 & year<=1899,1,0)) %>%
  mutate(north20war= ifelse(war ==1 &north ==1 & year>1899,1,0)) %>%
  group_by(stdata) %>%
  mutate(stfatperday =sum(northfat)) %>%
  mutate(stwarperday = sum(northwar)) %>%
  mutate(stwar19perday=sum(north19war)) %>%
  mutate(stwar20perday=sum(north20war)) %>%
  ungroup()

mid <- mid %>%
  group_by(strtmnth) %>%
  mutate(stwar19permon= sum(north19war)) %>%
  mutate(stwar20permon=sum(north20war)) %>%
  mutate(stterrmidpermon=sum(terr)) %>%
  mutate(stnonmidpermon=sum(nonterr)) %>%
  mutate(stmidpermon=sum(north)) %>%
  mutate(stfatpermon=sum(northfat)) %>%
  mutate(stwarpermon=sum(northwar)) %>%
  ungroup() %>%
  #northnot seems to be a meausure of which MIDs did not have fatalities and where in the north
  #so edmidperday is not which fatalities ended on that day
  mutate(northnot=ifelse(north==1 & northfat !=1,1,0)) %>%
  group_by(enddata) %>%
  mutate(edmidperday=sum(northnot)) %>%
  mutate(edfatperday=sum(northfat)) %>%
  mutate(edwarperday=sum(northwar)) %>%
  mutate(edwarall=sum(war)) %>%
  ungroup() %>%
  group_by(endmnth) %>%
  mutate(edmidpermon=sum(north)) %>%
  mutate(edfatpermon=sum(northfat)) %>%
  mutate(edwarpermon=sum(northwar))
 
###
mid_fig1 <- mid %>% arrange(strtmnth) %>% 
  group_by(strtmnth) %>% slice(1)

##Create Figure 1 in the paper
#### bad attempts
#f<- mfp(data = mid, stwarpermon ~ fp(strtmnth))
#summary(f)

p<- ggplot(mid_fig1, aes(x= strtmnth,y= stwarpermon)) +
  geom_point() +
  #geom_function(stat=f)+
  stat_smooth(method = "lm", formula = y ~ I((x/10)^1) + I((x/10)^2))+
  ggtitle("MID War Onsets per Month")+ 
  labs(color="")+
  ylab("Number of War Onsets per Month")+ xlab("Month of Year") +
  xlim(1,12) + ylim(-20,80)
p
p+ theme_stata()

###best attempt at figure 1   
fig1 <- ggplot(mid_fig1, aes(x= strtmnth,y= stwarpermon)) +
  geom_point() +
  #geom_function(stat=f)+
  #geom_spline(spar = .5)+
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x), aes(fill = after_scale(color)), alpha = 0.2) +
  ggtitle("MID War Onsets per Month")+ 
  labs(color="")+
  ylab("Number of War Onsets per Month")+ xlab("Month of Year") +
  xlim(1,12) + ylim(-20,80)
fig1 + theme_classic()

 
 
#mfp(strtmnth ~ fp(stwarpermon, df=1), family = glm, data = mid)
#mfp


#### Figure 2
mid_fig2 <- mid %>% group_by(stdata) %>% slice(1)
fig2<- ggplot(data = mid_fig2, aes(y = stwarperday, x = stdata)) +
  geom_point() +
  geom_smooth( method = 'gam')
fig2

### Figure 3
#the stata code
#comment: mids per day of year
#This is Figure 3 in the paper (version 07082019)
mid_fig3 <- mid %>% mutate(dummy = 1) %>%
  group_by(strtday) %>%
  mutate(stdaycount = sum(dummy)) %>%
  slice(1)

fig3 <- ggplot(data = mid_fig3, aes(x = strtday, y = stdaycount)) +
  geom_bar(stat="identity")+ 
  labs(color="")+
  ylab("Number of MID Onsets by Day of Month")+ 
  xlab("Day of Month") 
fig3  

### Figure 4
#comment: conflict onset by day, MIDs, fatal MIDs, and Wars
#This is Figure 4 in paper (version 07082019)
#uses same edited data as fig 2

fig4 <- ggplot(data=mid_fig2, aes(y = stmidperday, x= stdata)) +
  geom_point() +
  xlab("Day of Year") + ylab("Number of MID Onsets per Day")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  scale_x_continuous(breaks=seq(1,365,30))
fig4

### Figure 5
mid_fig5 <- mid %>% group_by(enddata) %>% slice(1)
#comment: conflict termination by day, MIDs, fatal MIDs, and Wars
#This is Figure 5 in the paper (version 07082019)
fig5 <- ggplot(data=mid_fig5, aes(y = edmidperday, x= enddata)) +
  geom_point() +
  xlab("Day of Year") + ylab("Number of MID Terminations per Day")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  scale_x_continuous(breaks=seq(1,365,30))
fig5

##saving the figures as pdf images
ggsave(paste0("Fig1", ".pdf"), plot = fig1)
ggsave(paste0("Fig2", ".pdf"), plot = fig2)
ggsave(paste0("Fig3", ".pdf"), plot = fig3)
ggsave(paste0("Fig4", ".pdf"), plot = fig4)
ggsave(paste0("Fig5", ".pdf"), plot = fig5)

### use data from merged file
mid <- mid %>% 
  arrange(statea, stateb, year) %>%
  mutate(dyadidyr = case_when(
    year<2000 ~ (statea*1000000)+(stateb*1000)+(year-1000),
    year>=2000 ~ (statea*1000000)+(stateb*1000)+(year-2000)
  )) %>%
  relocate(dyadidyr, .before = statea)

dir_dyad <- read_csv("/Users/galinaisayeva/RA/Seasonality_of_conflict/merge_data/directed_dyad_18162010.csv") %>% select(-c(year))

mid <- left_join(mid, dir_dyad, by = "dyadidyr") %>%
  mutate(stdata2=stdata*stdata,  enddata2=enddata*enddata, year2=year*year)


#merge on polity data
#twice
pol_1 <- read_xls("p5v2018.xls") %>%
  select(c(ccode, year, polity, polity2)) 
names(pol_1) <- paste0(names(pol_1), "1")
pol_1 <- pol_1 %>%
  rename(ccode_1 = ccode1, year= year1)

#clean polity data second time
pol_2 <- read_xls("p5v2018.xls") %>%
  select(c(ccode, year, polity, polity2)) 
names(pol_2) <- paste0(names(pol_2), "2")
pol_2 <- pol_2 %>%
  rename(ccode_2 = ccode2, year= year2)

#here, import dataset from MIDs that you use
keys <- read_dta("dyadic_mid_4.01/dyadic_mid_4.01.dta") %>%
  select(statea, stateb, year) %>%
  unique() %>%
  rename(ccode_1 = statea, ccode_2 = stateb)

#merge on keys
pol_rev_a<- left_join(keys, pol_1, by = c("ccode_1", "year")) %>% left_join(pol_2, by = c("ccode_2", "year")) %>%
  rename(statea = ccode_1, stateb = ccode_2) %>%
  mutate(dyadidyr = case_when(
    year<2000 ~ (statea*1000000)+(stateb*1000)+(year-1000),
    year>=2000 ~ (statea*1000000)+(stateb*1000)+(year-2000)
  )) %>%
    relocate(dyadidyr, .before = statea)

#pol_rev_b can be omitted. Origianlly used to try to replace missing values. But original code produces no changes in data
pol_rev_b <- pol_rev_a %>%
  select(-c(dyadidyr)) %>%
  rename(bstatea = statea, bstateb = stateb, polityxx = polity1, bpolity1 = polity2, polityyy = polity21, bpolity21 = polity22) %>%
  mutate(dyadidyr = case_when(
    year<2000 ~ (bstatea*1000000)+(bstateb*1000)+(year-1000),
    year>=2000 ~ (bstatea*1000000)+(bstateb*1000)+(year-2000)
  )) %>%
  relocate(dyadidyr, .before = bstatea)

season_data <- left_join(mid, pol_rev_a, by = c("dyadidyr")) %>% left_join(pol_rev_b, by = c("dyadidyr")) %>%
  #original code had lines to replace potentially missing values, but it does not change anything
  select(-c(bstatea, bstateb, bpolity1, bpolity21))

#data manipulation
season_data <- season_data %>% 
  mutate(demautai=((polity21+10)/2), demautbi=((polity22+10)/2), demautinter=demautai*demautbi) %>%
  apply_labels(demautai ="DEMi - polity21(j. hewitt)+10/2", demautbi= "DEMi - polity22(j. hewitt)+10/2", demautinter= "DEMi - demautai*demautbi")

#comment: lower of dyadic democracy scores

season_data <- season_data %>%
  mutate(demloi= case_when(
    demautai<=demautbi & !is.na(demautai) & !is.na(demautbi) ~ demautai,
    demautai>demautbi & !is.na(demautai) & !is.na(demautbi) ~ demautbi,
    TRUE ~NA_real_)) %>%
  apply_labels(demloi= "DEMi - Lower of interp. dyadic scores") %>%
#higher of dyadic dem scores
  mutate(demhii= case_when(
    demautai>demautbi & !is.na(demautai) & !is.na(demautbi) ~ demautai,
    demautai<=demautbi & !is.na(demautai) & !is.na(demautbi) ~ demautbi,
    TRUE ~NA_real_)) %>%
  apply_labels(demhii ="DEMi - Higher of interp. dyadic scores")

#check code below works
#preparing capability data
season_data <- season_data %>%
  mutate(cinclo = case_when(
    cap_1<=cap_2 & !is.na(cap_1) & !is.na(cap_2) ~ cap_1,
    cap_1>cap_2 & !is.na(cap_1) & !is.na(cap_2) ~ cap_2,
    TRUE ~ NA_real_
  ),
  cinchi =case_when(
    cap_1<=cap_2 & !is.na(cap_1) & !is.na(cap_2) ~ cap_2,
    cap_1>cap_2 & !is.na(cap_1) & !is.na(cap_2) ~ cap_1,
    TRUE ~ NA_real_),
  cincratio = (cinclo/(cinclo+cinchi))) %>%
  apply_labels(cinclo ="CINC - Lower of dyadic scores", cinchi ="CINC - Higher of dyadic scores", cincratio= "CINC - ratio of capabilities"
)

#preparing alliance data
season_data <- season_data %>%
  mutate(alliance = case_when(
    alliance ==5 ~0,
    TRUE ~ alliance
  )) %>%
  mutate(onemajor = case_when(
    majpow_1 ==1 | majpow_2 ==1 ~ 1
    !is.na(majpow_1) & !is.na(majpow_2) ~ 0
  ),
  allydumy = case_when(
    alliance >0 & !is.na(alliance) ~ 1,
    is.na(alliance) ~ NA_real_,
    TRUE ~ 0
  ),
  defdummy = case_when(
    alliance ==1 ~ 1,
    is.na(alliance) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  apply_labels(allydumy ="ALLIANCE - dummy for all alliance types", defdummy ="ALLIANCE - dummy for defense pact")

#logging distance
season_data <- season_data %>% 
  mutate(logdist = ln(distance +1)) %>%
  apply_labels(logdist = "ln(distance +1)") %>%
  #constructing energy as a proxy for development
  mutate(irstpopa=irst_1/tpop_1, irstpopb=irst_2/tpop_2, engypopa=pec_1/tpop_1, engypopb=pec_2/tpop_2) %>%
  mutate(irstpop = case_when(
    irstpopa<irstpopb &!is.na(irstpopa)& !is.na(irstpopb) ~irstpopa,
    irstpopa>=irstpopb &!is.na(irstpopa)& !is.na(irstpopb) ~irstpopb
  ),
  engypop= case_when(
    engypopa<engypopb &!is.na(engypopa)& !is.na(engypopb) ~engypopa,
    engypopa>=engypopb &!is.na(engypopa)& !is.na(engypopb) ~engypopb
  )) %>%
    apply_labels(irstpop= "Dev. proxy, lower of irst/tpop", engypop= "Dev. proxy, lower of energy/tpop", engypopl= "Dev. proxy, lower of ln(energy/tpop)")


