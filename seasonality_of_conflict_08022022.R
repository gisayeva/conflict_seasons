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
library(MASS)
library(estimatr)
library(stargazer)
#setwd("")
#load and clean MID location dataset
temploc <- read_csv("MIDLOCA_2.1.csv")
temploc<- rename(temploc, disno = dispnum)
temploc <- dplyr::select(temploc, -c(year, midloc2_location, midloc2_measuringpoint))

#testing how branches work

#load and clean mid dataset
mid <- read_dta("dyadic_mid_4.01.dta") %>% 
  dplyr::select(-c("namea", "nameb")) %>% 
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
  dplyr::select(-c(smonthday))

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
  dplyr::select(-c(emonthday))
  
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
 

######
##Figure 1
######
###
mid_fig1 <- mid %>% arrange(strtmnth) %>% 
  group_by(strtmnth) %>% slice(1)

##Create Figure 1 in the paper
fig1 <- ggplot(mid_fig1, aes(x= strtmnth,y= stwarpermon)) +
  geom_point() +
  #geom_function(stat=f)+
  #geom_spline(spar = .5)+
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x), aes(fill = after_scale(color)), alpha = 0.2) +
  ggtitle("MID War Onsets per Month")+ 
  labs(color="")+
  ylab("Number of War Onsets per Month")+ xlab("Month of Year") +
  ylim(-20,80)+ 
  scale_x_continuous(breaks=seq(1,12,1))+
  theme_clean()
fig1+theme_bw()
fig1
 
########
#### Figure 2
#######
mid_fig2 <- mid %>% group_by(stdata) %>% slice(1)
fig2<- ggplot(data = mid_fig2, aes(y = stwarperday, x = stdata)) +
  geom_point(color = "black") +
  geom_smooth( method = 'gam',colour="dark red", se =FALSE)+
  ggtitle("War Onsets per Day (Northern Hemisphere)")+ 
  labs(color="")+
  ylab("Number of War Onsets per Day")+ xlab("Day of Year")+
  theme_clean()
fig2

#######
### Figure 3
#######
mid_fig3 <- mid %>% mutate(dummy = 1) %>%
  group_by(strtday) %>%
  mutate(stdaycount = sum(dummy)) %>%
  slice(1)

fig3 <- ggplot(data = mid_fig3, aes(x = strtday, y = stdaycount)) +
  geom_bar(stat="identity", fill= "light blue")+ 
  labs(color="")+
  ylab("Number of MID Onsets")+ 
  xlab("Day of Month")+
  ggtitle("MID Onsets by Day of Month") +
  theme_clean()
fig3 

#######
### Figure 4
#######
#comment: conflict onset by day, MIDs, fatal MIDs, and Wars
#uses same edited data as fig 2

fig4 <- ggplot(data=mid_fig2, aes(y = stmidperday, x= stdata)) +
  geom_point() +
  xlab("Day of Year") + ylab("Number of MID Onsets")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se= F, color = "dark red") +
  scale_x_continuous(breaks=seq(1,365,30))+
  ggtitle("MID Onsets per Day (Northern Hemisphere)") +
  theme_clean()
fig4

#######
### Figure 5
#######
mid_fig5 <- mid %>% group_by(enddata) %>% slice(1)
#comment: conflict termination by day, MIDs, fatal MIDs, and Wars
fig5 <- ggplot(data=mid_fig5, aes(y = edmidperday, x= enddata)) +
  geom_point() +
  xlab("Day of Year") + ylab("Number of MID Terminations per Day")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = F, color = "dark red") +
  scale_x_continuous(breaks=seq(1,365,30)) +
  ggtitle("MID Terminations per Day (Northern Hemisphere")+
  theme_clean()
fig5 


##saving the figures as pdf images
ggsave(paste0("Fig1", ".pdf"), plot = fig1)
ggsave(paste0("Fig2", ".pdf"), plot = fig2)
ggsave(paste0("Fig3", ".pdf"), plot = fig3)
ggsave(paste0("Fig4", ".pdf"), plot = fig4)
ggsave(paste0("Fig5", ".pdf"), plot = fig5)



######
### use data from merged file
mid <- mid %>% 
  arrange(statea, stateb, year) %>%
  mutate(dyadidyr = case_when(
    year<2000 ~ (statea*1000000)+(stateb*1000)+(year-1000),
    year>=2000 ~ (statea*1000000)+(stateb*1000)+(year-2000)
  )) %>%
  relocate(dyadidyr, .before = statea)

dir_dyad <- read_csv("directed_dyad_18162010.csv") %>% dplyr::select(-c(year))

mid <- left_join(mid, dir_dyad, by = "dyadidyr") %>%
  mutate(stdata2=stdata*stdata,  enddata2=enddata*enddata, year2=year*year)


#merge on polity data
#twice
pol_1 <- read_xls("p5v2018.xls") %>%
  dplyr::select(c(ccode, year, polity, polity2)) 
names(pol_1) <- paste0(names(pol_1), "1")
pol_1 <- pol_1 %>%
  rename(ccode_1 = ccode1, year= year1)

#clean polity data second time
pol_2 <- read_xls("p5v2018.xls") %>%
  dplyr::select(c(ccode, year, polity, polity2)) 
names(pol_2) <- paste0(names(pol_2), "2")
pol_2 <- pol_2 %>%
  rename(ccode_2 = ccode2, year= year2)

#here, import dataset from MIDs that you use
keys <- read_dta("dyadic_mid_4.01.dta") %>%
  dplyr::select(statea, stateb, year) %>%
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
  dplyr::select(-c(dyadidyr)) %>%
  rename(bstatea = statea, bstateb = stateb, polityxx = polity1, bpolity1 = polity2, polityyy = polity21, bpolity21 = polity22) %>%
  mutate(dyadidyr = case_when(
    year<2000 ~ (bstatea*1000000)+(bstateb*1000)+(year-1000),
    year>=2000 ~ (bstatea*1000000)+(bstateb*1000)+(year-2000)
  )) %>%
  relocate(dyadidyr, .before = bstatea)

season_data <- left_join(mid, pol_rev_a, by = c("dyadidyr")) %>% left_join(pol_rev_b, by = c("dyadidyr")) %>%
  #original code had lines to replace potentially missing values, but it does not change anything
  dplyr::select(-c(bstatea, bstateb, bpolity1, bpolity21))

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
    majpow_1 ==1 | majpow_2 ==1 ~ 1,
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
  mutate(logdist = log(distance +1)) %>%
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
  ),
  engypopl=case_when(
    engypopa<engypopb & !is.na(engypopa) &!is.na(engypopb) ~log(engypopa +1),
    engypopa>=engypopb & !is.na(engypopa) &!is.na(engypopb) ~log(engypopb +1)
  )) %>%
    apply_labels(irstpop= "Dev. proxy, lower of irst/tpop", engypop= "Dev. proxy, lower of energy/tpop", engypopl= "Dev. proxy, lower of ln(energy/tpop)")

season_data <- season_data %>%
  mutate(capinter=cap_1*cap_2,
         summilper=((milper_1/1000)+(milper_2/1000)),
         diffmilper=abs((milper_1/1000)-(milper_2/1000)),
         summilex=((milex_1/1000)+(milex_2/1000)),
         diffmilex=abs((milex_1/1000)-(milex_2/1000)),
         sumengy=((pec_1/1000)+(pec_2/1000)),
         diffengy=abs((pec_1/1000)-(pec_2/1000)),
         capsum=cap_1+cap_2,
         capdiff=abs(cap_1-cap_2),
         polityinter=polity21*polity22,
         engypopinter=engypopa*engypopb) %>%
  mutate(absdiff=abs(176-stdata),
         absno1diff=abs(176-no1stdata),
         absend=abs(203-enddata),
         absno1end=abs(203-no1enddata)
         )
#comment: 176 is the median value of stdata
#comment:  NA is the median value of no1stdata
#comment: 203 is the median value of enddata


######
##Create table 1
######
season_data_t1a<- season_data %>% arrange(stdata) %>%
  group_by(stdata) %>% slice(1) 
season_data_t1b<- season_data %>% arrange(enddata) %>%
  group_by(enddata) %>% slice(1)
#figure out how to/whether add robust errors and cluster on disno
t1_m1<- glm.nb(stmidperday ~ stdata+ stdata2, data = season_data_t1a)
t1_m2<- glm.nb(stfatperday ~stdata +stdata2, data = season_data_t1a)
t1_m3<- glm.nb(stwarperday ~ stdata+ stdata2, data = season_data_t1a)
t1_m4<- glm.nb(edwarall~ enddata +enddata2, data = season_data_t1b)
t1_m5<- glm.nb(edfatperday ~enddata+ enddata2, data = season_data_t1b)
t1_m6<- glm.nb(edmidperday ~enddata +enddata2, data = season_data_t1b)

t1a<- stargazer(t1_m1, t1_m2, t1_m3, title = "Predicting the Timing of MIDs by the Day of Year", type = "text", covariate.labels = c("Day of Year", "Day of Year Squared", 'Intercept'), dep.var.labels = c("All MIDs", "Fatal MIDs", "MID Wars"), out= "tab1a.tex")
t1b<- stargazer(t1_m4, t1_m5, t1_m6, title = "Predicting the Timing of MIDs by the Day of Year", type = "text", covariate.labels = c("Day of Year", "Day of Year Squared", 'Intercept'), dep.var.labels = c("MID Wars", "Fatal MIDs", "Non-fatal MIDs"), out= "tab1b.tex")

#######
##make table 2
######
##the 6 models
##variable named numstate in Stata version of code
t2_m1 <- lm(latitude ~ stdata +stdata2 +year +year2+ numstates +numGPs, data= season_data, subset =(north ==1))
t2_m2 <- lm(latitude ~ stdata +stdata2 +year +year2+ numstates +numGPs, data= season_data, subset =(northwar ==1))
t2_m3 <- lm(latitude ~ no1stdata +no1stdata2 +year+ year2+defdummy+ cap_1+ cap_2 +capinter+ demautai+ demautbi+ demautinter+ colcont +logdist+ numstates +numGPs, data= season_data, subset =(north ==1))
t2_m4 <- lm(latitude ~ enddata +enddata2 +year+ year2+defdummy+ cap_1+ cap_2 +capinter+ demautai+ demautbi+ demautinter+ colcont +logdist+ numstates +numGPs, data= season_data, subset =(north ==1))
t2_m5 <- lm(latitude ~ stdata +stdata2 +year+ year2+defdummy+ cap_1+ cap_2 +capinter+ demautai+ demautbi+ demautinter+ colcont +logdist+ numstates +numGPs, data= season_data, subset =(north ==1 & terr ==1))
t2_m6 <- lm(latitude ~ stdata +stdata2 +year+ year2+defdummy+ cap_1+ cap_2 +capinter+ demautai+ demautbi+ demautinter+ colcont +logdist+ numstates +numGPs, data= season_data, subset =(north ==1 & nonterr ==1))
#t2<- stargazer(t2_m1, t2_m2, t2_m3, t2_m4, t2_m5, t2_m6, title = "The Effect of Seasonal Change on the Latitude of Militarized Interstate Disputes", type = "latex", covariate.labels = c("Day of Year", "Day of Year^2","Day of Year adjusted", "Day of Year adjusted^2","Day of Year (end)", "Day of Year (end)^2", "Year", "Year^2", "Alliance (dummy)", "CINC_{A}","CINC_{B}","CINC_{A \times B}", "Democracy_{A}", "Democracy_{B}", "Dem_{A \times B}", "Colonial Contiguity", "Logged distance", "# States", "# Great Powers", "Constant"), out= "tab2")
t2 <- stargazer(t2_m1, t2_m2, t2_m3, t2_m4, t2_m5, t2_m6, title = "The Effect of Seasonal Change on the Latitude of Militarized Interstate Disputes", type = "latex", covariate.labels = c("Day of Year", "Day of Year2","Day of Year adjusted", "Day of Year adjusted2","Day of Year (end)", "Day of Year (end)2", "Year", "Year2", "Alliance (dummy)", "CINC A","CINC B","CINC A x B", "Democracy A", "Democracy B", "Dem A x B", "Colonial Contiguity", "Logged distance", "num States", "num Great Powers", "Constant"), out= "tab2.tex")

######
##make table 3
######
t3_m1<- lm(absno1diff ~ distance+ year+ numstates +numGPs, data= season_data)
t3_m2 <- lm(absno1diff ~ distance +recip +year +cap_1 +cap_2 +capinter +colcont +defdummy +numstates+ numGPs, data = season_data)
t3_m3 <- lm(absdiff ~ distance +recip+ year+ cap_1 +cap_2+ capinter+ demautai +demautbi+ demautinter +engypopa +engypopb+ engypopinter+ onemajor+ colcont +allydumy +numstates+ numGPs, data = season_data)
t3_m4 <-  lm(absno1end ~ distance +recip +year +cap_1 +cap_2 +capinter +colcont +defdummy +numstates+ numGPs, data = season_data)
t3_m5 <- lm(absno1end ~distance+ year +cap_1 +cap_2 +capinter+ demautai+ demautbi+ demautinter+ engypopa +engypopb+ engypopinter+ onemajor +colcont +defdummy +numstates+ numGPs, data= season_data, subset= (war ==1))
t3<- stargazer(t3_m1, t3_m2, t3_m3, t3_m4, t3_m5, title = "The Effect of Distance on the Timing of MIDs", type = "latex", covariate.labels = c("Distance", "Reciprocated", "Year", "CINC$_{A}$","CINC$_{B}$","CINC$_{A \times B}$", "Democracy$_{A}$", "Democracy$_{B}$", "Dem$_{A \times B}$", "Devolopment$_{A}$", "Development$_{B}$", "Dev$_{A \times B}$", "Major Power","Colonial Contiguity", "Defense (dummy)", "Alliance", "# States", "# Great Powers"), dep.var.labels = c("Abs(day-mean(day))", "Abs(day-mean(day))", "Abs(day-mean(day))"), out= "tab3.tex")


###########
## create figure 6
### attempt 9/15/22
#t2_m3 <- lm(latitude ~ no1stdata +no1stdata2 +year+ year2+defdummy+ cap_1+ cap_2 +capinter+ demautai+ demautbi+ demautinter+ colcont +logdist+ numstates +numGPs, data= season_data, subset =(north ==1))
##set all variables to median
## set  no1stdata and no1stdata2 to 1 - 365
##run predict
mid_fig6 <- season_data %>%
  ungroup() %>%
  mutate(year = median(year, na.rm = T),
         year2 = median(year2, na.rm = T),
         defdummy = median(defdummy, na.rm = T),
         cap_1 = median(cap_1, na.rm = T),
         cap_2 = median(cap_2, na.rm = T),
         capinter = median(capinter, na.rm = T),
         demautai = median(demautai, na.rm = T),
         demautbi = median(demautbi, na.rm = T),
         demautinter = median(demautinter, na.rm = T),
         colcont = median(colcont, na.rm = T),
         logdist = median(logdist, na.rm = T),
         numstates = median(numstates, na.rm = T),
         numGPs = median(numGPs, na.rm = T)) %>%
  slice_head(n=365) %>%
  dplyr::select(c(year, year2,defdummy, cap_1, cap_2,capinter, demautai, demautbi, demautinter, colcont, logdist, numstates, numGPs))

mid_fig6$no1stdata <- c(1:365)  
mid_fig6 <- mid_fig6 %>%
  mutate(no1stdata2 = no1stdata^2)

#predict new latitude for onset of MIDs based on median values for all variables except for day of onset, and day^2
mid_fig6$pred <- predict(t2_m3, newdata = mid_fig6, interval = "confidence")

fig6 <- ggplot(data = mid_fig6) +
  geom_smooth(aes(x = c(1:365), y = pred[,1]), color = "black" , size = .7) +
  geom_smooth(aes(x = c(1:365), y = pred[,2]), color= "lightblue", linetype = 2) +
  geom_smooth(aes(x = c(1:365), y = pred[,3]), color= "lightblue", linetype = 2) +
  ylab("Predicted Latitude of MID onsets")+
  xlab("Day of Year")+
  scale_x_continuous(breaks=seq(1,365,30)) +
  scale_y_continuous(breaks = seq(20,32,1)) +
  ggtitle("Latitude Predicted by Day in the Northern Hemisphere")+
  theme_clean()
fig6
ggsave(paste0("fig6", ".pdf"), plot = fig6)

######
##new figure: Figure 7
##aggregate data by year
mid_fig7<- season_data %>% ungroup() %>% distinct(disno, .keep_all = T)
fig7_all <- ggplot(mid_fig7) +
  geom_bar(aes(x = strtyr)) +
  scale_x_continuous(breaks = seq(1810,2010,20))+
  xlab("Year") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Year, 1812-2010") +
  theme_bw()
fig7_all
fig7a <- ggplot(subset(mid_fig7, strtyr %in% c(0:1850))) +
  geom_bar(aes(x = strtmnth)) +
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab("Month") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Month, 1812-1850")+
  theme_bw()
fig7a
fig7b <- ggplot(subset(mid_fig7, strtyr %in% c(1850:1900))) +
  geom_bar(aes(x = strtmnth)) +
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab("Month") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Month, 1850-1900")+
  theme_bw()
fig7b
fig7c <- ggplot(subset(mid_fig7, strtyr %in% c(1900:1950))) +
  geom_bar(aes(x = strtmnth)) +
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab("Month") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Month, 1900-1950")+
  theme_bw()
fig7c
fig7d <- ggplot(subset(mid_fig7, strtyr %in% c(1950:2000))) +
  geom_bar(aes(x = strtmnth)) +
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab("Month") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Month, 1950-2000")+
  theme_bw()
fig7d
fig7e <- ggplot(subset(mid_fig7, strtyr %in% c(2000:2050))) +
  geom_bar(aes(x = strtmnth)) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  xlab("Month") + ylab("Number of Dispute Onsets") +
  ggtitle("Number of Dispute Onsets per Month, 2000-2010")+
  theme_bw()
fig7e

# ggsave(paste0("fig7_all", ".pdf"), plot = fig7_all)
# ggsave(paste0("fig7a", ".pdf"), plot = fig7a)
# ggsave(paste0("fig7b", ".pdf"), plot = fig7b)
# ggsave(paste0("fig7c", ".pdf"), plot = fig7c)
# ggsave(paste0("fig7d", ".pdf"), plot = fig7d)
# ggsave(paste0("fig7e", ".pdf"), plot = fig7e)

