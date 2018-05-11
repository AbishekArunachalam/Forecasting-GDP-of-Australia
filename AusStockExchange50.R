library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
ASX50 <- read_excel("ASX 50 (XFL).xlsx",col_types = c("date","numeric","numeric","numeric","numeric","numeric"),range = cell_cols("A:F"))
ASX50 <- ASX50[-(1:5),]
ASX50$`S&P/ASX 50 (XFL)` <- as.Date(ASX50$`S&P/ASX 50 (XFL)`)
ASX50 <- ASX50[,c(1,6)]
names(ASX50) <- c("Date","ASX50change")
qtrDate <- quarter(ASX50$Date,with_year = T,fiscal_start = 1) #Allocate months to quarters(Lubridate package)
qtrDate <- gsub(".1","-Q1",qtrDate,fixed = T) #Substitute quarter numbers to explicit characters
qtrDate <- gsub(".2", "-Q2", qtrDate, fixed = T)
qtrDate <- gsub(".3", "-Q3", qtrDate, fixed = T)
qtrDate <- gsub(".4", "-Q4", qtrDate, fixed = T)
ASX50 <- ASX50 %>% mutate(qtrDate) #Add new column containing quarter to data frame 
ASX50 <- ASX50[,-which(names(ASX50) %in% c("Date"))]
ASX50 <- ASX50 %>% group_by(qtrDate) %>% summarize(ASX50change=mean(ASX50change)) #Group by year quarters and calculate sum
ASX50 <- separate(ASX50, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
colnames(ASX50)[3] <- "ASX50(%change)"
head(ASX50)