# conflict_seasons
Code for the Seasonality of Conflict paper

This repository contains 3 files and 7 datasets.

This README file.

seasonality_merge.R, an R script file.

seasonality_of_conflict_08022022.R, an R script file.

The necessary datasets to run the code and produce the output.

First, run seasonality_merge.R, which will create a large dataset with observations on the dyadic level from 1816 to 2010. The variables include number of states in the system, number of great powers, great power status of the states in the dyad, and data on capability, contiguity, colonial contiguity, alliances, as well as distance (between capitals). They are sourced from a variety of datasets, citations and sources (will be) at the bottom of the README file. The output is a csv file titled "directed_dyad_18162010.csv".

Then, run seasonality_of_conflict_08022022.R, which will use the MID 4.01 dataset, the MID location 2.1 dataset, the directed_dyad_18162010.csv dataset created above, and the Polity V dataset. The output is the 6 figures and 3 tables from the paper. It also creates an additional set of graphs, that show number of MID conflicts per month per 50 year period. 

Citations

Alliance dataset v4.1: 
D.M. Gibler. International Military Alliances, 1648-2008. Congressional Quarterly Press. 2009. ISBN: 978-1-56802-824-8

Distance (cepii):
Mayer, T. & Zignago, S. (2011), Notes on CEPII’s distances measures : the GeoDist Database, CEPII Working Paper 2011-25

Colonial contiguity v3.1:
Correlates of War Project. Colonial Contiguity Data, 1816-2016. Version 3.1.

Direct contiguity v3.2:
Correlates of War Project. Direct Contiguity Data, 1816-2016. Version 3.2.

NMC v6.0:
Singer, J. David, Stuart Bremer, and John Stuckey. (1972). “Capability Distribution, Uncertainty, and Major Power War, 1820-1965.” in Bruce Russett (ed) Peace, War, and Numbers, Beverly Hills: Sage, 19-48.

State System v2016:
Correlates of War Project. 2016. “State System Membership List, v2016.” Online, http://correlatesofwar.org.

Dyadic MID dataset v4.01:
Zeev Maoz, Paul L. Johnson, Jasper Kaplan, Fiona Ogunkoya, and Aaron Shreve 2019. The Dyadic Militarized Interstate Disputes (MIDs) Dataset Version 3.0: Logic, Characteristics, and Comparisons to Alternative Datasets, Journal of Conflict Resolution, 6(3) 811-835.

MID Location v2.1:
Braithwaite, A. 2010. MIDLOC: Introducing the Militarized Interstate Dispute Location dataset. Journal of Peace Research, 47(1), 91-98.

Bezerra, P., Braithwaite, A. 2019. Codebook for the Militarized Interstate Dispute Location (MIDLOC-A/I) Dataset, v2.1.

Polity V:
Marshall, M. G. & Gurr, T. R. Polity5: Political Regime Characteristics and Transitions, 1800-2018 (Dataset Users’ Manual). 85 (2020).

Other citations:

Hlavac M (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. Social Policy Institute, Bratislava, Slovakia. R package version 5.2.3, https://CRAN.R-project.org/package=stargazer.


