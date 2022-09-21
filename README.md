# conflict_seasons
Code for the Seasonality of Conflict paper //
This repository contains 3 files. //
This README file.//
seasonality_merge.R, an R script file. //
seasonality_of_conflict_08022022.R, an R script file.//
The necessary datasets.//

First, run seasonality_merge.R, which will create a large dataset with observations on the dyadic level from 1816 to 2010. The variables include number of states in the system, number of great powers, great power status of the states in the dyad, and data on capability, contiguity, colonial contiguity, alliances, as well as distance (between capitals). They are sourced from a variety of datasets, citations and sources (will be) at the bottom of the README file. The output is a csv file titled "directed_dyad_18162010.csv".//

Then, run seasonality_of_conflict_08022022.R, which will use the MID 4.01 dataset, the MID location 2.1 dataset, the directed_dyad_18162010.csv dataset created above, and the Polity V dataset. The output is the 6 figures and 3 tables from the paper. It also creates an additional set of graphs, that show number of MID conflicts per month per 50 year period. 
