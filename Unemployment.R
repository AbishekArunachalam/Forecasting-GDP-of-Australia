library(dplyr)
library(tidyr)
unemployment <- read.csv("DP_LIVE_28042018200453939.csv")
unemployment.AUS <- filter(unemployment,LOCATION =="AUS")
unemployment.AUS <- unemployment.AUS[,c("TIME","Value")]
unemployment.AUS <- separate(unemployment.AUS, TIME, into = c("Year", "Quarter"), sep="-")                             
colnames(unemployment.AUS)[3] <- c("Percentage unemployed %")
write.csv(file="unemployment.csv",unemployment.AUS)
