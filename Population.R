library(rsdmx)
library(dplyr)
library(tidyr)
url = "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.TT.Q/all?startTime=1981-Q3&endTime=2017-Q3"
population <- readSDMX(url,dsd = T)
population <- as.data.frame(population)
head(population)
population<- population %>% filter(STATE=='0')
population <- population[,c("obsTime","obsValue")]
population <- population %>% separate(obsTime, into = c("Year", "Quarter"), sep="-")
#Add values manually from ABS
#24,930,937,- 
population.latest1 <-data.frame(Year ='2017',Quarter='Q4',obsValue=24814851)
population.latest2 <-data.frame(Year ='2018',Quarter='Q1',obsValue=24910737)
rbind(population,population.latest1,population.latest2)



