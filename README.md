# Introduction

Gross Domestic Product(GDP) of a country provides a measure of the total
monetary value of all the goods and services it produces during a certain time
period. It is a most popular measure to gauge the health of an economy. A
constant growth in GDP without large fluctuations signify that the country is
performing well economically. The factors that contribute to the growth of GDP
varies between countries.

GDP= Consumption or Consumer spending (C) + Government spending (G) + 
Investment of country (I) + Business capital expenditures (NX).

My team gathered important indicators that substituted the formula and we created the master dataset.
During the initial analysis we found that Human Development Index (HDI), Inventories, Interest rates and 
Population were important influencers that contribute to the Australian GDP. 

Time-series models with different parameter settings were run on the dataset:

* ARMA
* ARIMA
* SARIMA
* Simple Exponential Smoothing
* Holt Winter's Exponential Smoothing (Additive method) 
* TBATS

TBATS gave the best forecast accuracy on the test set with a Mean Average Error(MAE) of 0.0085. 
