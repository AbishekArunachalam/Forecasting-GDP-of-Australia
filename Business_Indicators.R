library(rsdmx)
library(dplyr)
library(tidyr)

url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/QBIS/10+50+90+110.TOTAL.0.99.10+20+30.Q/all?startTime=1985-Q1&endTime=2017-Q4"
business_indicators <- readSDMX(url,dsd = T)
business_indicators <- as.data.frame(business_indicators)
business_indicators<- business_indicators[,c("MEASURE","TSEST","obsTime","obsValue")]
View(business_indicators)

unique(business_indicators$MEASURE)
# Measure - 10-Sales($ Million), 50-Inventories($ Million), 90-Wages, 110 - Gross Operating Profits
unique(business_indicators$TSEST)
#TSEST - 10-Original, 20-Seasonally Adjusted, 30-Trend
business_indicators$MEASURE[business_indicators$MEASURE == '10'] <- "Sales($ Million)"
business_indicators$MEASURE[business_indicators$MEASURE == '50'] <- "Inventories($ Million)"
business_indicators$MEASURE[business_indicators$MEASURE == '90'] <- "Wages"
business_indicators$MEASURE[business_indicators$MEASURE == '110'] <- "Gross Operating Profit"
unique(business_indicators$MEASURE)
business_indicators %>% count(MEASURE) #Inventories seem to be inconsistent with others
business_indicators$MEASURE <- as.factor(business_indicators$MEASURE)

# Sales------------------------------------------------------

Sales <- dplyr::filter(business_indicators,MEASURE == "Sales($ Million)")
Sales$TSEST[Sales$TSEST== '10'] <- "Original" #132
Sales$TSEST[Sales$TSEST== '20'] <- "Seasonally adjusted" #68
Sales$TSEST[Sales$TSEST == '30'] <- "Trend" #68
unique(Sales$TSEST)
Sales.Original <- filter(Sales, TSEST == "Original")
Sales.SeasonallyAdjusted <- filter(Sales, TSEST == "Seasonally adjusted")
Sales.Trend <- filter(Sales, TSEST =="Trend")
Sales.Original <- Sales.Original[-c(1,2)]

# Inventories--------------------------------------------------

Inventories <- dplyr::filter(business_indicators,MEASURE == "Inventories($ Million)")
Inventories$TSEST[Inventories$TSEST == '10'] <- "Original" #132
Inventories$TSEST[Inventories$TSEST == '20'] <- "Seasonally adjusted" #130
Inventories$TSEST[Inventories$TSEST == '30'] <- "Trend" #130
Inventories.Original <- filter(Inventories, TSEST == "Original")$obsValue
Inventories.SeasonallyAdjusted <- filter(Inventories, TSEST == "Seasonally adjusted")
Inventories.Trend <- filter(Inventories, TSEST == "Trend")

# Wages---------------------------------------------------------

Wages <- dplyr::filter(business_indicators,MEASURE == "Wages")
Wages$TSEST[Wages$TSEST == '10'] <- "Original" #132
Wages$TSEST[Wages$TSEST == '20'] <- "Seasonally adjusted" #68
Wages$TSEST[Wages$TSEST == '30'] <- "Trend" #68
Wages.Original <- filter(Wages, TSEST == "Original")$obsValue
Wages.SeasonallyAdjusted <- filter(Wages, TSEST == "Seasonally adjusted")
Wages.Trend <- filter(Wages, TSEST == "Trend")

# GOP ----------------------------------------------------------

GOP <- dplyr::filter(business_indicators,MEASURE == "Gross Operating Profit")
GOP$TSEST[GOP$TSEST == '10'] <- "Original" #132
GOP$TSEST[GOP$TSEST == '20'] <- "Seasonally adjusted" #68
GOP$TSEST[GOP$TSEST == '30'] <- "Trend" #68
#GOP- profit subtracting the operating expenses.
GOP.Original <- filter(GOP, TSEST == "Original")$obsValue
GOP.SeasonallyAdjusted <- filter(GOP, TSEST == "SeasonallyAdjusted")
GOP.Trend <- filter(GOP, TSEST == "Trend")

#Expenditure----------------------------------------------------

url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CAPEX/1.999.-.0.10+20+30.Q/all?startTime=1987-Q2&endTime=2017-Q4"
Expenditure <- as.data.frame(readSDMX(url))
Expenditure$TSEST[Expenditure$TSEST == '10'] <- "Original" #132
Expenditure$TSEST[Expenditure$TSEST == '20'] <- "Seasonally adjusted" #68
Expenditure$TSEST[Expenditure$TSEST == '30'] <- "Trend" #68
Expenditure.Original <- filter(GOP, TSEST == "Original")$obsValue
Expenditure.SeasonallyAdjusted <- filter(GOP, TSEST == "SeasonallyAdjusted")
Expenditure.Trend <- filter(GOP, TSEST == "Trend")

# Business_Indicator master dataset---------------------------------

business_indicators <- as.data.frame(cbind(Sales.Original,Inventories.Original,Wages.Original,GOP.Original,Expenditure.Original))
colnames(business_indicators) <-  c("Date","Sales($ Million)","Inventories($ Million)","Wages","GOP","Expenditure($ Million)")
business_indicators <- separate(business_indicators, Date, into = c("Year", "Quarter"), sep="-")




