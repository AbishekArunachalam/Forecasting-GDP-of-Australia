library(readxl)
library(dplyr)
library(lubridate)
Balance_on_goods_and_services <- read_excel("536802.xls", 
                                            sheet = "Data1", col_types = c("date", 
                                                                           "text"),range = cell_cols("A:B"))

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
View(Balance_on_goods_and_services)
write.csv(file = "balance_on_goods_and_services.csv",Balance_on_goods_and_services)






