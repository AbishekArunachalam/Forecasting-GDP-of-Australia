library("lubridate")
options(stringsAsFactors = FALSE)
install.packages("rsdmx")
library(rsdmx)
library(magrittr)
library(tidyr)
library(ggplot2)
providers <- getSDMXServiceProviders()
p<-as.data.frame(providers)

# SDMX  -------------------------------------------------------------------
rm(list=ls())

# Consumer Price Index ----------------------------------------------------

cpi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001+131199+999901.10+20.Q/all?startTime=1948-Q3&endTime=2018-Q1"
cpi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/CPI"

read_cpidata<-readSDMX(cpi_url)
read_cpidsd<-readSDMX(cpi_dsd)
setcpi<- setDSD(read_cpidata,read_cpidsd)
cpi_df<- as.data.frame(setcpi)
head(cpi_df)
unique(cpi_df$MEASURE)
unique(cpi_df$INDEX)
unique(cpi_df$OBS_STATUS)
cpi_tidydf<- cpi_df %>% select(-REGION,-MEASURE,-FREQUENCY,-TSEST,-TIME_FORMAT,-OBS_STATUS)%>% spread(INDEX,obsValue)%>%separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(cpi_tidydf)
names(cpi_tidydf)[3:5]<- c("CPI","CPI_indirectcharges_loan&deposit","CPI_seasonallyadjusted")

# House Price Index -------------------------------------------------------

hpi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/RES_PROP_INDEX/1.3+2+1.100.Q/all?startTime=2002-Q1&endTime=2017-Q4"

hpi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/RES_PROP_INDEX"

read_hpidata<- readSDMX(hpi_url)
read_hpidsd<- readSDMX(hpi_dsd)
sethpi<- setDSD(read_hpidata,read_hpidsd)
hpi_df<- as.data.frame(sethpi)
hpi_tidydf<-hpi_df %<>% select(-MEASURE,-ASGS_2011,-FREQUENCY,-TIME_FORMAT)
head(hpi_tidydf)
hpi_tidydf<- separate(hpi_tidydf, obsTime, into = c("Year", "Quarter"), sep="-")  
hpi_tidydf<- spread(hpi_tidydf,PROP_TYPE,obsValue)
names(hpi_tidydf)[3:5]<- c("HPI_residentprop","HPI_establishedhouse","HPI_dwelling")

# Export Price Index ANZSIC ----------------------------------------------------

epi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ITPI_EXPORT/1.8123922+8123923+8123924.Q/all?startTime=1974-Q3&endTime=2018-Q1"

epi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ITPI_EXPORT"

read_epidata<- readSDMX(epi_url)
read_epidsd<- readSDMX(epi_dsd)
setepi<-setDSD(read_epidata,read_epidsd)
epi_df<-as.data.frame(setepi)
head(epi_df)
unique(epi_df$TIME_FORMAT)
epi_tidydf<- epi_df %>% select(-MEASURE,-FREQUENCY,-TIME_FORMAT) %>% spread(INDEX,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(epi_tidydf)
names(epi_tidydf)[3:5]<- c("EXPI_ANZSIC_agri_forest_fishing","EXPI_ANZSIC_Mining","EXPI_ANZSIC_Manufacturing")


# SITC --------------------------------------------------------------------
#EXPORT
expi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ITPI_EXPORT/1.8093697.Q/all?startTime=1974-Q3&endTime=2018-Q1"
expi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ITPI_EXPORT"


read_expidata<- readSDMX(expi_url)
read_expidsd<- readSDMX(expi_dsd)
setexpi<-setDSD(read_expidata,read_expidsd)
expi_df<-as.data.frame(setexpi)
head(expi_df)
expi_tidydf<- expi_df %>% select(-MEASURE,-FREQUENCY,-TIME_FORMAT) %>% spread(INDEX,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(expi_tidydf)
names(expi_tidydf)[3]<- c("EXPI_SITC")

#IMPORT
imi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ITPI_IMPORT/1.6011001.Q/all?startTime=1981-Q3&endTime=2018-Q1"
imi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ITPI_IMPORT"

read_imidata<- readSDMX(imi_url)
read_imidsd<- readSDMX(imi_dsd)
setimi<-setDSD(read_imidata,read_imidsd)
imi_df<-as.data.frame(setimi)
head(imi_df)
imi_tidydf<- imi_df %>% select(-MEASURE,-FREQUENCY,-TIME_FORMAT) %>% spread(INDEX,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(imi_tidydf)
names(imi_tidydf)[3]<- c("IMPI_SITC")

# Import Price Index ANZSIC -----------------------------------------------

impi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ITPI_IMPORT/1.8124037+8124038+8124039.Q/all?startTime=1981-Q3&endTime=2018-Q1"

impi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ITPI_IMPORT"

read_impidata<- readSDMX(impi_url)
read_impidsd<- readSDMX(impi_dsd)
setimpi<- setDSD(read_impidata,read_impidsd)
impi_df<- as.data.frame(setimpi)
head(impi_df)
impi_tidydf<- impi_df %>% select(-MEASURE,-FREQUENCY,-TIME_FORMAT) %>% spread(INDEX,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(impi_tidydf)
names(impi_tidydf)[3:5]<- c("IMPI_ANZSIC_agri_forest_fishing","IMPI_ANZSIC_Mining","IMPI_ANZSIC_Manufacturing")


# Wage Price Index --------------------------------------------------------

wpi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.10+20+30.Q/all?startTime=1997-Q3&endTime=2017-Q4"

wpi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/LABOUR_PRICE_INDEX"

read_wpidata<- readSDMX(wpi_url)
read_wpidsd<- readSDMX(wpi_dsd)
setwpi<-setDSD(read_wpidata,read_wpidsd)
wpi_df<- as.data.frame(setwpi)
head(wpi_df)
unique(wpi_df$INDEX)
wpi_tidydf<- wpi_df %>% select(-MEASURE,-SECTOR,-FREQUENCY,-REGION,-INDEX,-INDUSTRY,-TIME_FORMAT) %>% spread(TSEST,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(wpi_tidydf)
names(wpi_tidydf)[3:5]<- c("WPI","WPI_seasonal","WPI_trend")


# Production Price Index --------------------------------------------------

ppi_url<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/PPI_SOP/1.T.THREE.TOT.TOTIE+TOTXE.Q/all?startTime=1997-Q3&endTime=2017-Q4"
ppi_dsd<- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/PPI_SOP"

read_ppidata<- readSDMX(ppi_url)
read_ppidsd<- readSDMX(ppi_dsd)
setppi<-setDSD(read_ppidata,read_ppidsd)
ppi_df<-as.data.frame(setppi)

head(ppi_df)
unique(ppi_df$DESTINATION)
ppi_tidydf<- ppi_df %>% select(-MEASURE,-SOURCE,-FREQUENCY,-STAGE,-INDEX,-TIME_FORMAT) %>% spread(DESTINATION,obsValue) %>% separate(obsTime, into = c("Year", "Quarter"), sep="-") 
head(ppi_tidydf)
names(ppi_tidydf)[3:4]<- c("PPI_incl_exports","PPI_excl_exports")



# Final DataFrame ---------------------------------------------------------

getwd()
final_df<-cpi_tidydf %>% left_join(hpi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join(epi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join(expi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join (imi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join(impi_tidydf,by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join (wpi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter")) %>% left_join(ppi_tidydf, by = c("Year" = "Year", "Quarter" = "Quarter"))
head(final_df)
write.csv(file="priceindex.csv",final_df)



