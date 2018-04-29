library(dplyr)
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
View(HDI)
write.csv(file="human_development_index.csv",HDI)
