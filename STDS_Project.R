library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(car)
library(GGally)
GDP.Master <- read.csv("GDP_Master.csv") # NOTE: the whole code was written for old master file.
head(GDP.Master,n=30)
#GDP.Master <- cbind(YearQTR =paste(GDP.Master$Year,GDP.Master$Quarter,sep=" "),GDP.Master)
GDP.By.Year <- aggregate(GDP.Master$GDP.US..Millions.,by=list(GDP.Master$Year),FUN=mean,na.rm=T)
tail(GDP.By.Year)
GDP.By.Year <- GDP.By.Year[-49,]
names(GDP.By.Year) <- c("Year","GDP")
#GDP ($ millions by year)
ggplot(GDP.By.Year, mapping = aes(x=Year,y=GDP,group=1,colour="red"))+
  geom_line()+
  geom_point()+xlab("Year")+ylab("GDP($ millions)")+
  theme(axis.text.x = element_text(size  = 10,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1))
GDP_fit_df <- GDP.Master[c(125:192),-which(names(GDP.Master) %in% c("X","Year","Quarter","HDI..."))]
fit1 <- lm(formula = GDP.US..Millions.~.,data=GDP_fit_df)
summary(fit1)
alias(fit1)
vif(fit1)

#Taking only to valid data entires for the model.
#2001-125
fit2 <- lm(formula = GDP.US..Millions.~Consumer.Price.Index.CPI.+Wages...Million.+Expenditure...Million.+Inventories...Million.+Labourforce,data = GDP_fit_df)
summary(fit2)
alias(fit2)

#
my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}
a <- GDP.Master[,-c(1,2,3)]
ggpairs(data = a,lower = list(continuous = my_fn))

library(leaps)
regfit.full <- regsubsets(GDP.US..Millions.~.,data = GDP_fit_df)
summary(regfit.full)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
which.min(reg.summary$rss)
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
points(8,reg.summary$rss[8],col="red",cex=2,pch=20)
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(3,reg.summary$adjr2[3], col="red",cex=2,pch=20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(reg.summary$cp )
points(2,reg.summary$cp [2],col="red",cex=2,pch=20)
which.min(reg.summary$bic )
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(1,reg.summary$bic [1],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

regfit.fwd=regsubsets (GDP.US..Millions.~.,data=GDP_fit_df, method ="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets (GDP.US..Millions.~.,data=GDP_fit_df,method ="backward")
summary(regfit.bwd)

train <- sample(c(TRUE,FALSE), nrow(GDP_fit_df),rep=TRUE,prob = c(0.7,0.3))
test =(! train )
regfit.best=regsubsets(GDP.US..Millions.~.,data=GDP_fit_df[train,])
test.mat=model.matrix(GDP.US..Millions.~.,data=GDP_fit_df[test,])

val.errors=rep(NA,8)
 for(i in 1:8){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((GDP_fit_df$GDP.US..Millions.[test]-pred)^2)
 }
#2 variable model is less.
coef(regfit.best ,2) # Consumer.Price.Index.CPI. 17435.00 X3.month.Monthly.Average.Interest.Rates... -3069.93 
#################################
library(dplyr)
library(tidyr)
GDP_Quaterly <- read.csv("DP_LIVE_08052018081745619.csv")
head(GDP_Quaterly)
AUS_GDP <- filter(GDP_Quaterly,LOCATION =="AUS")
head(AUS_GDP)
AUS_GDP <- AUS_GDP[,c("TIME","Value")]
ggplot(data = AUS_GDP,mapping = aes(TIME,Value))+geom_smooth(method = lm)+geom_point()+
  theme(axis.text.x = element_text(size  = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1))
AUS_GDP <- separate(AUS_GDP,TIME, into = c("Year", "Quarter"), sep="-")
AUS_GDP <- cbind(YearQTR = paste(AUS_GDP$Year,AUS_GDP$Quarter,sep =" "),AUS_GDP)
AUS_GDP <- AUS_GDP[-c(1:39),]
GDP.Master <- read.csv("GDP_Master.csv")
GDP.Master <- GDP.Master[-c(193),]
AUS_GDP <- cbind(AUS_GDP,GDP.Master$GDP.US..Millions.,Percentage.Unemployed =GDP.Master$Percentage.unemployed..)
AUS_GDP$Value <- as.numeric(AUS_GDP$Value) 
AUS_GDP$Year <- as.numeric(AUS_GDP$Year)
GDP.Agg <-aggregate(AUS_GDP$Value, by=list(AUS_GDP$Year), 
                    FUN=mean, na.rm=TRUE)
names(GDP.Agg) <- c("Year","GDP")
UnEmployment.Agg<-aggregate(AUS_GDP$Percentage.Unemployed, by=list(AUS_GDP$Year), 
                            FUN=mean, na.rm=TRUE)
df <- cbind(UnEmployment = UnEmployment.Agg$x,GDP.Agg)
df <- df %>%
  select(Year,UnEmployment,GDP) %>%
  gather(key = "variable", value = "value", -Year)
head(df, 3)
# Multiple line plot
ggplot(df, aes(x = Year, y = value,group=1))+ 
  geom_area(aes(color = variable, fill = variable,group=factor(variable)),alpha=0.5,position=position_dodge(0.8))+
  scale_color_manual(values = c("#F7B800", "#00AFBB"))+
  scale_fill_manual(values = c("#F7B800", "#00AFBB"))+ #000000 D55E00-red
  ylab("Percentage(%)")
