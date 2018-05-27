#get OECD Countries Quarterly Expense Approach GDP from 1980 to present
library(rsdmx)
library(tidyverse)
#dataurl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+ARG+BRA+CHN+COL+CRI+IND+IDN+LTU+RUS+SAU+ZAF.B1_GE.CPCARSA.Q/all?startTime=1960-Q1&endTime=2018-Q1"
dataurl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS+IRL+FRA+NLD+KOR+CAN+USA+GBR+JPN+ESP.B1_GE.CPCARSA.Q/all?startTime=1960-Q1&endTime=2018-Q1"
OECDGDP <- as.data.frame(readSDMX(dataurl,dsd = T))

OECDGDP <- OECDGDP[,c("LOCATION","obsTime","obsValue")]
colnames(OECDGDP)[1] <- "Country"
colnames(OECDGDP)[2] <- "Quarter"
colnames(OECDGDP)[3] <- "GDP_USD_Millions"
OECDGDP <- OECDGDP %>% mutate(changepct = 0)
#check for NA if sum returns a value
sum(OECDGDP$GDP_USD_Millions)
#value was returned, no NA, proceed next step
OECDGDP <- OECDGDP %>% arrange(Country,Quarter)
GDPChangePct <- OECDGDP[0,]

Countries <- unique(OECDGDP$Country)
countryname <- 0
line <- 0

#compute change percentage
for (countryname in 1:length(Countries))
  {
    CountryGDP <- OECDGDP %>% filter(Country == Countries[countryname])
    for (line in 2:nrow(CountryGDP))
      {
        CountryGDP[line,]$changepct <- 100*(CountryGDP[line,]$GDP_USD_Millions - CountryGDP[line-1,]$GDP_USD_Millions)/CountryGDP[line-1,]$GDP_USD_Millions
                  
      }  
  GDPChangePct <- rbind(GDPChangePct,CountryGDP)  
  }

#compute change percentage finished

#save to file once
#write.csv(file = "GDPChangePct.csv",GDPChangePct)

#count the number of consecutive positive growth for each country
#define GDP growth counter table
GDPGrowthCount <- data.frame(CountryName=character(), GrowthCount=integer())
names(GDPGrowthCount) <- c("CountryName","GrowthCount")

for (countryname in 1:length(Countries))
  {
  
    CountryGDP <- GDPChangePct %>% filter(Country == Countries[countryname]) %>% arrange(Country,Quarter)
  
    posgdpcount <- 0
    posgrowthcount <- 0
    prevgdpgrowth <- CountryGDP[1,]$changepct

    for (line in 2:nrow(CountryGDP))
      {
        if (CountryGDP[line,]$changepct > 0 || CountryGDP[line,]$changepct < 0 && prevgdpgrowth > 0)
          {
            if (CountryGDP[line,]$changepct > 0)
              {
                posgrowthcount <- posgrowthcount + 1
              }
            prevgdpgrowth <- CountryGDP[line,]$changepct
          }
        else 
          {
            if (posgrowthcount > posgdpcount)
              {
                posgdpcount <- posgrowthcount
              }
            posgrowthcount <- 0
          }
      }
      print(paste(posgdpcount, posgrowthcount))      
      #insert new row
      newrow <- data.frame(Countries[countryname], posgdpcount)
      names(newrow) <- c("CountryName","GrowthCount")
      GDPGrowthCount <- rbind(GDPGrowthCount, newrow)
    
  }
    
GDPGrowthCount <- GDPGrowthCount %>% arrange(desc(GrowthCount))

#write.csv(file = "GDPGrowthCount.csv",GDPGrowthCount)

View(GDPChangePct)
#View(GDPGrowthCount)

library(ggplot2)

ggplot(GDPChangePct,
       aes(GDPChangePct$Quarter, changepct,
       group=Country, color=Country)) + geom_line() + xlab('Quarter') +
       theme(axis.text.x=element_text(angle=45, hjust=1))


ggplot(GDPGrowthCount, aes(x=reorder(CountryName, -GrowthCount), y=GrowthCount)) +
  geom_bar(stat="identity", width=0.5 )
