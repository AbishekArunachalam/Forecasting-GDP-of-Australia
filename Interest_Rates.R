library(readxl)
library(lubridate)
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
interestRates <- interestRates %>% group_by(qtrDate) %>% summarize(Interest_Rates= mean(`Interest rates`))
interestRates <- separate(interestRates, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
View(interestRates)
colnames(interestRates)[3] <- "3-month Monthly Average Interest Rates(%)"
write.csv(file="interest_rates.csv",interestRates)
