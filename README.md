# Quarterly age and sex stratified death rates in Denmark from 2020 to 2022 (WIP)

### Summary

In this analysis, I calculated age and sex stratified death rates (deaths per population size) for each quarter during the Covid-19 pandemic in Denmark (2020 Q1 to, so far, 2022 Q3) and compared them to pre-pandemic death rates using two different baselines. The baselines were established by linear regression followed by adjustment for seasonality. Excess deaths were defined as deaths exceeding a prediction interval (1.96 $\times$ standard deviation using the chosen baseline period) around the season adjusted baselines. This analysis therefore takes into account both long term trends, age composition, and seasonality. With this method, I find 81 and 62 excess deaths per 100,000 (~4800 and ~3700 excess deaths, respectively) from 2020 Q1 to 2022 Q3 using the 2010-19 and 2015-19 baseline, respectively. Finally, I explore how sensitive the result is to choice of baseline (using 2008-19, 2009-19, 2010-19, 2011-19, 2012-19, 2013-19, 2014-19, 2015-19, and 2016-19 baselines) and choice of data stratification (varying age bin size and sex stratification), testing 45 combinations in total.
