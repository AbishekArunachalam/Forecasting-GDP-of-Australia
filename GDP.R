library(rsdmx)
library(tidyr)
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS.B1_GE.CPCARSA.Q/all?startTime=1960-Q1&endTime=2018-Q1"
GDP <- readSDMX(url,dsd = T)
str(GDP)
GDP <- GDP[,c("obsTime","obsValue")]
head(GDP)
GDP <- separate(GDP,obsTime, into = c("Year", "Quarter"), sep="-")
colnames(GDP)[3] <- "GDP(US$ Millions)"
View(GDP)
write.csv(file = "GDP.csv",GDP)
