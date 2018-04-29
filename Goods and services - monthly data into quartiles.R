library(readxl)
library(dplyr)

Balance_on_goods_and_services <- read_excel("Balance on goods and services (5368m) Column B.xls", 
                                                     sheet = "Data1", col_types = c("date", 
                                                                                    "text", "text", "text", "text", "text", 
                                                                                    "text", "text", "text", "text", "text", 
                                                                                    "text", "text", "text", "text", "text"))



# create data-frame with desired rows and columns from original dataset
mydata <- Balance_on_goods_and_services[-c(1:9),-c(3:16)]
str(mydata)
names(mydata)
# Rename columns
colnames(mydata)[colnames(mydata)=="X__1"] <- "Date"
colnames(mydata)[colnames(mydata)=="Balance on goods and services ;"] <- "Balance_on_goods_and_services"

# transform data types
mydata$Date <- as.Date(mydata$Date)
mydata$Balance_on_goods_and_services <- as.numeric(mydata$Balance_on_goods_and_services)

# produce a Time-Series object for aggregation of data into Quartiles
monthly <- ts(mydata,start=c(1971,7),frequency=12)
#str(monthly)
#summary(monthly)
monthly[,2]

quarterly <- aggregate(monthly, nfrequency=4,mean)
quarterly[,2]
quarterly

# transform Quartile time-series object into data-frame
Goods_and_Services_quarters <- as.data.frame(quarterly)
# add year and quarter variable from time-series index
library(zoo) #may need to load library zoo for below
Goods_and_Services_quarters$qtr <- as.yearqtr(index(quarterly))
# transform quarter variable from class yearqtr to string
Goods_and_Services_quarters <- Goods_and_Services_quarters %>%
  mutate(qtr = paste0(substring(year(qtr),1,4),"-Q",quarter(qtr))) 

#checkS -- note Date column is now redundant as it is a calculated mean
Goods_and_Services_quarters
str(Goods_and_Services_quarters)
