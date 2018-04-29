library(readxl)
library(lubridate)
Exchange_Rates_1969_2009 <- read_excel("f11hist-1969-2009.xls",sheet = "Data",col_types = c("date","text","text"),range = cell_cols("A:C"))
Exchange_Rates_2010_present <- read_excel("f11hist.xls",sheet = "Data",col_types = c("date","text","text"),range = cell_cols("A:C"))
head(Exchange_Rates_1969_2009)
head(Exchange_Rates_2010_present)
Exchange_Rates_1969_2009 <- Exchange_Rates_1969_2009[-c(1:10),-2]
Exchange_Rates_2010_present <- Exchange_Rates_2010_present[-c(1:10),-3]
str(Exchange_Rates_1969_2009)
names(Exchange_Rates_1969_2009)
colnames(Exchange_Rates_1969_2009) <- c("Date","Exchange Rate(AU$1=USD)")
colnames(Exchange_Rates_2010_present) <- c("Date","Exchange Rate(AU$1=USD)")
Exchanges_Rates <- rbind(Exchange_Rates_1969_2009,Exchange_Rates_2010_present)
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
Exchanges_Rates <- Exchanges_Rates %>% group_by(qtrDate) %>% summarize('Exchange Rate(AU$1=USD)' = mean(`Exchange Rate(AU$1=USD)`)) #Group by year quarters and calculate mean.
Exchanges_Rates <- separate(Exchanges_Rates, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
View(Exchanges_Rates)
write.csv(file = "exchange_rate.csv",Exchanges_Rates)
