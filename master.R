
# ----------------------GDP Prediction ------------------------------------

# ------------------Master Datasheet --------------------------------------
# First Steps loading Required Packages -----------------------------------

#rm(list=ls())
check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
     install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages<-c("ggplot2", "rsdmx", "magrittr", "tidyr", "readxl", "dplyr","lubridate") #required packages
check.packages(packages) # Calling check.packages function

options(stringsAsFactors = FALSE)

# SDMX --------------------------------------------------------------------

providers <- getSDMXServiceProviders()
p<-as.data.frame(providers)

getwd()

# Balance of goods and Services in Millions -------------------------------

Balance_on_goods_and_services <- read_excel("536802.xls", 
                                            sheet = "Data1", col_types = c("date", 
                                                                           "text"),range = cell_cols("A:B"))
View(mydata)
# create data-frame with desired rows and columns from original dataset
mydata <- Balance_on_goods_and_services[-c(1:9),]
str(mydata) # Balance is in char format and date is in POSIXct format
names(mydata)
# Rename columns
colnames(mydata) <- c("Date","Balance_on_goods_and_services")

mydata$Date <- as.Date(mydata$Date) #Convert to date format
mydata$Balance_on_goods_and_services <- as.numeric(mydata$Balance_on_goods_and_services) # Convert Balance to numeric

qtrDate <- quarter(mydata$Date,with_year = T,fiscal_start = 1) #Allocate months to quarters(Lubridate package)
qtrDate <- gsub(".1","-Q1",qtrDate,fixed = T) #Substitute quarter numbers to explicit characters
qtrDate <- gsub(".2", "-Q2", qtrDate, fixed = T)
qtrDate <- gsub(".3", "-Q3", qtrDate, fixed = T)
qtrDate <- gsub(".4", "-Q4", qtrDate, fixed = T)
mydata <- mydata %>% mutate(qtrDate) #Add new column containing quarter to data frame 
mydata <- mydata[,-1] #Remove date column
mydata <- mydata %>% group_by(qtrDate) %>% summarize(Balance_on_goods_and_services = sum(Balance_on_goods_and_services)) #Group by year quarters and calculate sum
Balance_on_goods_and_services <- separate(mydata, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
colnames(Balance_on_goods_and_services)[3] <- "Balance_on_goods_and_services($ Million)"
head(Balance_on_goods_and_services)
glimpse(Balance_on_goods_and_services)
#write.csv(file = "balance_on_goods_and_services.csv",Balance_on_goods_and_services)


# Business Indicators -----------------------------------------------------

url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/QBIS/10+50+90+110.TOTAL.0.99.10+20+30.Q/all?startTime=1985-Q1&endTime=2017-Q4"
business_indicators <- readSDMX(url,dsd = T)
business_indicators <- as.data.frame(business_indicators)
business_indicators <- business_indicators[,c("MEASURE","TSEST","obsTime","obsValue")]
head(business_indicators)

unique(business_indicators$MEASURE)
# Measure - 10-Sales($ Million), 50-Inventories($ Million), 90-Wages, 110 - Gross Operating Profits
unique(business_indicators$TSEST)
#TSEST - 10-Original, 20-Seasonally Adjusted, 30-Trend
business_indicators$MEASURE[business_indicators$MEASURE == '10'] <- "Sales($ Million)"
business_indicators$MEASURE[business_indicators$MEASURE == '50'] <- "Inventories($ Million)"
business_indicators$MEASURE[business_indicators$MEASURE == '90'] <- "Wages"
business_indicators$MEASURE[business_indicators$MEASURE == '110'] <- "Gross Operating Profit"
unique(business_indicators$MEASURE)
#business_indicators %>% count(MEASURE) #Inventories seem to be inconsistent with others
#business_indicators$MEASURE <- as.factor(business_indicators$MEASURE)
business_indicators$TSEST[business_indicators$TSEST== '10'] <- "Original" #132
business_indicators$TSEST[business_indicators$TSEST== '20'] <- "Seasonally adjusted" #68
business_indicators$TSEST[business_indicators$TSEST == '30'] <- "Trend" #68
business_indicators_tidy_df<- business_indicators %>% dplyr::filter(TSEST=="Original") %>% 
    spread(MEASURE,obsValue) %>%  
    separate( obsTime, into = c("Year", "Quarter"), sep="-") %>%  
    select(-TSEST)
head(business_indicators_tidy_df)

#Expenditure----------------------------------------------------

url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CAPEX/1.999.-.0.10+20+30.Q/all?startTime=1987-Q2&endTime=2017-Q4"
Expenditure <- as.data.frame(readSDMX(url))
head(Expenditure)
unique(Expenditure$STATE)
Expenditure$TSEST[Expenditure$TSEST == '10'] <- "Original" #132
Expenditure$TSEST[Expenditure$TSEST == '20'] <- "Seasonally adjusted" #68
Expenditure$TSEST[Expenditure$TSEST == '30'] <- "Trend" #68

# Expenditure.Original <- filter(GOP, TSEST == "Original")$obsValue
# Expenditure.SeasonallyAdjusted <- filter(GOP, TSEST == "SeasonallyAdjusted")
# Expenditure.Trend <- filter(GOP, TSEST == "Trend")
head(Expenditure)
Expenditure_tidydf<- Expenditure %>% dplyr::filter(TSEST=="Original") %>% 
    separate( obsTime, into = c("Year", "Quarter"), sep="-") %>%  
    select(-TSEST,-EXP,-STATE,-IND,-FREQUENCY,-TIME_FORMAT,-OBS_STATUS,-ASSET) 
colnames(Expenditure_tidydf)[3] <- "Expenditure($ Million)"
head(Expenditure)

# Exchange Rates ----------------------------------------------------------

Exchange_Rates_1969_2009 <- readxl::read_excel("f11hist-1969-2009.xls",sheet = "Data",col_types = c("date","text","text"),range = cell_cols("A:C"))
Exchange_Rates_2010_present <- read_excel("f11hist.xls",sheet = "Data",col_types = c("date","text","text"),range = cell_cols("A:C"))
head(Exchange_Rates_1969_2009)
head(Exchange_Rates_2010_present)
Exchange_Rates_1969_2009 <- Exchange_Rates_1969_2009[-c(1:10),-2]
Exchange_Rates_2010_present <- Exchange_Rates_2010_present[-c(1:10),-3]
str(Exchange_Rates_1969_2009)
names(Exchange_Rates_1969_2009)
colnames(Exchange_Rates_1969_2009) <- c("Date","Exchange Rate(AU$1=USD)")
colnames(Exchange_Rates_2010_present) <- c("Date","Exchange Rate(AU$1=USD)")
Exchanges_Rates <- rbind(Exchange_Rates_1969_2009,Exchange_Rates_2010_present) # Adding Rows from both the sheets
Exchanges_Rates$Date <- as.Date(Exchanges_Rates$Date)
Exchanges_Rates$`Exchange Rate(AU$1=USD)` <- as.numeric(Exchanges_Rates$`Exchange Rate(AU$1=USD)`)
qtrDate <- quarter(Exchanges_Rates$Date,with_year = T,fiscal_start = 1) #Allocate months to quarters(Lubridate package)
qtrDate
qtrDate <- gsub(".1","-Q1",qtrDate,fixed = T) #Substitute quarter numbers to explicit characters
qtrDate <- gsub(".2", "-Q2", qtrDate, fixed = T)
qtrDate <- gsub(".3", "-Q3", qtrDate, fixed = T)
qtrDate <- gsub(".4", "-Q4", qtrDate, fixed = T)
Exchanges_Rates <- Exchanges_Rates %>% mutate(qtrDate) #Add new column containing quarter to data frame 
Exchanges_Rates <- Exchanges_Rates[,-1]
Exchanges_Rates_tidydf <- Exchanges_Rates %>% 
    group_by(qtrDate) %>% 
    summarize('Exchange Rate(AU$1=USD)' = mean(`Exchange Rate(AU$1=USD)`)) #Group by year quarters and calculate mean

Exchanges_Rates_tidydf <- separate(Exchanges_Rates_tidydf, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
head(Exchanges_Rates_tidydf)
#write.csv(file = "exchange_rate.csv",Exchanges_Rates)


# GDP ---------------------------------------------------------------------

url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS.B1_GE.CPCARSA.Q/all?startTime=1960-Q1&endTime=2018-Q1"
GDP <- as.data.frame(readSDMX(url,dsd = T))
str(GDP)

GDP <- GDP[,c("obsTime","obsValue")]
head(GDP,60)
GDP <- separate(GDP,obsTime, into = c("Year", "Quarter"), sep="-")
colnames(GDP)[3] <- "GDP(US$ Millions)"
View(GDP)

#write.csv(file = "GDP.csv",GDP)


# Human Development Index -------------------------------------------------

HumanDevelopmentIndex <- read.csv("Human Development Index (HDI).csv",header = F)
head(HumanDevelopmentIndex)
HumanDevelopmentIndex <- filter(HumanDevelopmentIndex,V2 == "Country" | V2==" Australia")
HumanDevelopmentIndex
Years <- 1990:2015
HDI <- as.numeric(HumanDevelopmentIndex[2,-c(1,2)])
HDI <- data.frame(Year = Years,HDI = HDI )
names(HDI)[2] <- "HDI(%)" 
HDI <- HDI[rep(seq_len(nrow(HDI)), each=4),]
rownames(HDI) <- c()
Quarter <- rep(c("Q1","Q2","Q3","Q4"),length(Years)) #Create quarters
HDI <- cbind(Quarter,HDI)
HDI <- HDI[,c(2,1,3)] #Reorder columns
summary(HDI)
HDI$Year<-as.character(HDI$Year)
head(HDI)
#write.csv(file="human_development_index.csv",HDI)


# Interest Rates ----------------------------------------------------------

interestRates <- read_excel("f01hist.xls",sheet = "Data",col_types = c("date","text","text","text","text","text"),range = cell_cols("A:F"))
interestRates <- interestRates[-c(1:10),c(1,5)]
head(interestRates,n=15)
#3-month BABs/NCDs Bank Accepted Bills/Negotiable Certificates of Deposit-3 months; monthly average
colnames(interestRates) <- c("Date","Interest rates")
interestRates$Date <- as.Date(interestRates$Date)
interestRates$`Interest rates` <- as.numeric(interestRates$`Interest rates`)
qtrDate <- quarter(interestRates$Date,with_year = T,fiscal_start = 1) #Allocate months to quarters(Lubridate package)
qtrDate
qtrDate <- gsub(".1","-Q1",qtrDate,fixed = T) #Substitute quarter numbers to explicit characters
qtrDate <- gsub(".2", "-Q2", qtrDate, fixed = T)
qtrDate <- gsub(".3", "-Q3", qtrDate, fixed = T)
qtrDate <- gsub(".4", "-Q4", qtrDate, fixed = T)
interestRates <- interestRates %>% mutate(qtrDate) #Add new column containing quarter to data frame 
interestRates <- interestRates[,-1]
interestRates <- interestRates %>% 
    group_by(qtrDate) %>% 
    summarize(Interest_Rates= mean(`Interest rates`))

interestRates <- separate(interestRates, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
head(interestRates)
colnames(interestRates)[3] <- "3-month Monthly Average Interest Rates(%)"
#write.csv(file="interest_rates.csv",interestRates)


# Unemployment Rate -------------------------------------------------------

unemployment <- read.csv("DP_LIVE_28042018200453939.csv",header = T)
names(unemployment)[1]<- "LOCATION"
unemployment.AUS <- filter(unemployment,LOCATION =="AUS")
unemployment.AUS <- unemployment.AUS[,c("TIME","Value")]
unemployment.AUS <- separate(unemployment.AUS, TIME, into = c("Year", "Quarter"), sep="-")   
colnames(unemployment.AUS)[3] <- c("Percentage unemployed %")
head(unemployment.AUS)
#write.csv(file="unemployment.csv",unemployment.AUS)

# Consumer Price Index ----------------------------------------------------

cpi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1+2+3.50.10001+999901.10+20.Q/all?startTime=1948-Q3&endTime=2018-Q1"
cpi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/CPI"

read_cpidata <-readSDMX(cpi_url)
read_cpidsd <-readSDMX(cpi_dsd)
setcpi <- setDSD(read_cpidata,read_cpidsd)
cpi_df <- as.data.frame(setcpi)
head(cpi_df)
unique(cpi_df$MEASURE) # 1- Index 2-Percentage change within quarters 3- Percentage change from corresponding quarter of Previous year
unique(cpi_df$TSEST) # 10- Original 20- Seasonal
unique(cpi_df$INDEX) # 10001 All groups CPI 999901 All groups Season data

# Fetching original CPI data only ---------------------------------------------

cpi_df<- cpi_df %>% filter(TSEST=="10",INDEX=="10001", MEASURE %in% c("1"))

cpi_tidydf <- cpi_df %>% 
    select(-REGION,-FREQUENCY,-TSEST,-TIME_FORMAT,-OBS_STATUS,-INDEX)%>% 
    spread(MEASURE,obsValue)%>%
    separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(cpi_tidydf)
names(cpi_tidydf)[3] <- c("Consumer Price Index(CPI)")
head(cpi_tidydf)

# Preparing Master Datasheet ----------------------------------------------

join_df <- cpi_tidydf %>% 
    left_join(business_indicators_tidy_df,by=c("Year" = "Year", "Quarter" = "Quarter")) %>% 
    left_join(Expenditure_tidydf,by=c("Year" = "Year", "Quarter" = "Quarter")) %>% 
    left_join(unemployment.AUS,by=c("Year" = "Year", "Quarter" = "Quarter"))  %>% 
    left_join(interestRates,by=c("Year" = "Year", "Quarter" = "Quarter")) %>% 
    left_join(HDI,by=c("Year" = "Year", "Quarter" = "Quarter")) %>% 
    left_join(GDP,by=c("Year" = "Year", "Quarter" = "Quarter")) %>%  
    left_join(Balance_on_goods_and_services,by=c("Year" = "Year", "Quarter" = "Quarter")) %>%  
    left_join(Exchanges_Rates_tidydf,by=c("Year" = "Year", "Quarter" = "Quarter"))

head(join_df)

# Filtering data from 1970 ------------------------------------------------

master_df <- join_df %>% filter(Year >= "1970")
write.csv(file="master.csv",master_df)

