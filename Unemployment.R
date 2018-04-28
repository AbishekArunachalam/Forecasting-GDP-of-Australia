library(tidyr)
unemployment <- read.csv("DP_LIVE_28042018200453939.csv")
unemployment <- unemployment[,c("TIME","Value")]
unemployment <- separate(unemployment, TIME, into = c("Year", "Quarter"), sep="-")                             
colnames(unemployment)[3] <- c("Percentage unemployed %")
write.csv(file="unemployment.csv",unemployment)