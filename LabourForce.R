Labourforce <- read_excel("6202001.xls", 
                          sheet = "Data1",skip=9,col_types="date",range=cell_cols("A"))
Labourforce1 <- read_excel("6202001.xls", 
                           sheet = "Data1",col_types="text",range=cell_cols("CY"))
# create data-frame with desired rows and columns from original dataset
Labourforce <- na.omit(Labourforce)
Labourforce1 <- Labourforce1[-c(1:9),]
Labourdata <- as.data.frame(cbind(Labourforce,Labourforce1))
names(Labourdata) <- c("Date","Labourforce") # Rename columns
str(Labourdata) 
Labourdata$Date <- as.Date(Labourdata$Date) #Convert to date format
Labourdata$Labourforce <- as.numeric(Labourdata$Labourforce) # Convert Balance to numeric

qtrDate <- quarter(Labourdata$Date,with_year = T,fiscal_start = 1) #Allocate months to quarters(Lubridate package)
qtrDate <- gsub(".1","-Q1",qtrDate,fixed = T) #Substitute quarter numbers to explicit characters
qtrDate <- gsub(".2", "-Q2", qtrDate, fixed = T)
qtrDate <- gsub(".3", "-Q3", qtrDate, fixed = T)
qtrDate <- gsub(".4", "-Q4", qtrDate, fixed = T)
Labourdata <- Labourdata %>% mutate(qtrDate) #Add new column containing quarter to data frame 
Labourdata <- Labourdata[,-1] #Remove date column
Labourdata <- Labourdata %>% 
  group_by(qtrDate) %>% 
  dplyr::summarize(mean(Labourforce)) #Group by year quarters and calculate sum
Labourdata <- Labourdata[-c(1:8,161),]
Labourdata <- separate(Labourdata, qtrDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
head(Labourdata)




trDate, into = c("Year", "Quarter"), sep="-") #Split quarter year column into two. One for year and other for quarter. 
head(interestRates)
colnames(interestRates)[3] <- "3-month Monthly Average Interest Rates(%)"