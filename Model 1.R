
### Model 1
rm(list=ls())
library(forecast)
library(caret)
library(plyr)
library("readxl")
library(Boruta)
set.seed(42)
setwd("C:/Users/benjamin/Desktop/36103/A2/Partb")

raw.data <- read_excel("5206036_expenditure_on_gdp_annual.xls", sheet = "Data1", col_types = "text",
                       range = cell_cols("A:AR"))

raw.data <- raw.data[-c(1:10),-c(1:5,8:26,28:35,37:40,43)]
colnames(raw.data) <- c("GovA","C","I","GovB","Exports","Imports","GDP")
raw.data$trend <- c(1:58)

raw.data$GovA <- as.numeric(raw.data$GovA)
raw.data$C <- as.numeric(raw.data$C)
raw.data$I <- as.numeric(raw.data$I)
raw.data$Exports <- as.numeric(raw.data$Exports)
raw.data$Imports <- as.numeric(raw.data$Imports)
raw.data$GovB <- as.numeric(raw.data$GovB)
raw.data$GDP <- as.numeric(raw.data$GDP)
raw.data$trend <- as.numeric(raw.data$trend)
head(raw.data)


    ### Test1
    temp <- raw.data
    timeSlices <- createTimeSlices(1:nrow(temp), initialWindow = round(nrow(temp)*0.7), horizon = round(nrow(temp)*0.3), fixedWindow = TRUE)
    min(data.frame(timeSlices[[2]]))
    max(data.frame(timeSlices[[1]]))
    train = temp[1:max(data.frame(timeSlices[[1]])),]
    test = temp[min(data.frame(timeSlices[[2]])):max(data.frame(timeSlices[[2]])),]
    train
    test
    
    model <- lm(GDP ~ ., data = train)
    summary(model)
    varImp(model)
    
    prediction <- predict(model,test)
    accuracy(prediction,test$GDP)
    
    plot(test$GDP, type = "l", lty = 1.8, col = "green")
    lines(prediction, type = "l", col = "blue")
    
    boruta.train <- Boruta(GDP~., data = train, doTrace = 2)
    print(boruta.train)
    plot(boruta.train, xlab = "", xaxt = "n")
    lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
      boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
    names(lz) <- colnames(boruta.train$ImpHistory)
    Labels <- sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
           at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

    ### Test2
    temp$G <- temp$GovA + temp$GovB
    temp$GovA <- NULL
    temp$GovB <- NULL
    temp$NX <- temp$Exports + temp$Imports
    temp$Exports <- NULL
    temp$Imports <- NULL
    head(temp)
    pie <- colSums(temp)
    pie <- data.frame(pie)
    pie$trend <- NULL
    pie$GDP <- NULL
    pie
    slices <- c(26433611, 8313972, 11036104, 13424663)
    lbls <- c("C", "I", "G", "NX")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices, labels = lbls)
    
    
    timeSlices <- createTimeSlices(1:nrow(temp), initialWindow = round(nrow(temp)*0.7), horizon = round(nrow(temp)*0.3), fixedWindow = TRUE)
    min(data.frame(timeSlices[[2]]))
    max(data.frame(timeSlices[[1]]))
    train = temp[1:max(data.frame(timeSlices[[1]])),]
    test = temp[min(data.frame(timeSlices[[2]])):max(data.frame(timeSlices[[2]])),]
    train
    test
    
    model <- lm(GDP ~ ., data = train)
    summary(model)
    varImp(model)
    
    prediction <- predict(model,test)
    prediction
    accuracy(prediction,test$GDP)
    
    plot(test$GDP, type = "l", lty = 1.8, col = "green")
    lines(prediction, type = "l", col = "blue")
    
    boruta.train <- Boruta(GDP~., data = train, doTrace = 2)
    print(boruta.train)
    plot(boruta.train, xlab = "", xaxt = "n")
    lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
      boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
    names(lz) <- colnames(boruta.train$ImpHistory)
    Labels <- sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
    
    
