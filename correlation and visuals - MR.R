library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)


data <- read.csv("master_new.csv")
head(data,n=5)
str(data)
summary(data)


# GDP yearly average
GDP_Year <- aggregate(data$GDP.US..Millions.,by=list(data$Year),FUN=mean,na.rm=T)
tail(GDP_Year)
GDP_Year <- GDP_Year[-39,]
names(GDP_Year) <- c("Year","GDP")

# avg GDP over years
ggplot(GDP_Year, aes(Year,GDP))+
  geom_line(color="blue") +  geom_point(color="purple")+
  xlab("Year")+ylab("GDP($US millions)")+
  geom_area(alpha =.3, fill="blue")+
  ggtitle(label="Gross Domestic Product (GDP) - US$ millions")



# unemployment rate yearly
unemp_rate_yr <- aggregate(data$Percentage.unemployed..,by=list(data$Year),FUN=mean,na.rm=T)
tail(unemp_rate_yr)
names(unemp_rate_yr) <- c("Year","Unemployment")

# avg unemployment rate over years
ggplot(unemp_rate_yr, aes(Year,Unemployment))+
  geom_line()+
  geom_point(colour="purple")+xlab("Year")+ylab("Percentage unemployed %")+
  geom_area(alpha = .3, fill="purple")+
  ggtitle(label="Unemployment Rate variance")

 

# population - yearly
population_yr <- aggregate(data$Tot_Population, by=list(data$Year),FUN=mean, na.rm=T)
names(population_yr) <- c("Year","Population")
# plot
ggplot(population_yr, aes(Year,Population))+
  geom_line()+
  geom_point(colour="purple")+xlab("Year")+ylab("Total Population")+
  geom_area(alpha=.3,fill="green")+
  ggtitle(label="Population Growth")+
  scale_y_continuous(labels = scales::comma)


install.packages("tidyquant")
library(tidyquant)
#library(broom)
#library(timekit)
#library(modelr)
#options(na.action = na.warn)

GDP_Year %>% 
  ggplot(aes(x = Year, y = GDP)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = palette_light()) +
  theme_tq()



#################################
# Australia GDP variance ViEW
#################################
#library(dplyr)
#library(tidyr)
GDP_Quaterly <- read.csv("DP_LIVE_08052018081745619.csv")
head(GDP_Quaterly)
colnames(GDP_Quaterly)[1] <- "LOCATION"

AUS_GDP <- filter(GDP_Quaterly,LOCATION =="AUS")
head(AUS_GDP)
AUS_GDP <- AUS_GDP[,c("TIME","Value")]
AUS_GDP <- separate(AUS_GDP,TIME, into = c("Year", "Quarter"), sep="-")

#shows trend variance of GDP by quarters over time
ggplot(data = AUS_GDP, aes(x = Year, y = Value, color = Quarter)) + geom_point() +
  geom_smooth(method = "auto") +
  scale_y_continuous(labels = scales::comma)+
  ggtitle(label="GDP variance rate over time")
  theme(axis.text.x = element_text(size  = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1))


#plot: variance spread over time
ggplot(data = AUS_GDP) +
  geom_line(mapping = aes(x=Year, y=Value)) +
  scale_y_continuous(labels = scales::comma)+ #to format display of y axis
  ggtitle(label="GDP variance spread over time")+
  theme(axis.text.x = element_text(size  = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1))

GDP.Agg <-aggregate(AUS_GDP$Value, by=list(AUS_GDP$Year), 
                    FUN=mean, na.rm=TRUE)
names(GDP.Agg) <- c("Year","GDP")
#plot: yearge average variance
ggplot(data = GDP.Agg, aes(x = Year, y = GDP)) + geom_point() +
  geom_smooth(method = "auto") +
  scale_y_continuous(labels = scales::comma)+
  ggtitle(label="GDP Yearly average variance rate over time")+
theme(axis.text.x = element_text(size  = 10,
                                 angle = 90,
                                 hjust = 1,
                                 vjust = 1))






######################################################
# EXPLORING CORRELATIONS
######################################################
#REMOVING non numeric columns for plotting correlations
data <- read.csv("master_new.csv")
mydata <- data[, c(4:19)]

#compute correlation matrix
# method = c("pearson", "kendall", "spearman") - pearson measures the linear dependence between two variables
# kendall and spearman correlation methods are non-parametric rank-based correlation test.
# If data contain missing values, use "complete.obs" to handle missing values by case-wise deletion.
data_cor <- cor(mydata, method = "pearson", use = "complete.obs")
round(data_cor, 2)


#correlation matrix with significance levels (p-values)
install.packages("Hmisc")
library("Hmisc")
data_cor2 <- rcorr(as.matrix(mydata))
data_cor2
# Extract the correlation coefficients
data_cor2$r
# Extract p-values
data_cor2$P


# ++++++++++++++++++++++++++++
# flattenCorrMatrix - simple function to format correlation matrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#using function on data_cor2 set
formatted_data_cor<-rcorr(as.matrix(data[,4:19]))
flatten_cor <- flattenCorrMatrix(formatted_data_cor$r, formatted_data_cor$P)


##############################
# VISUALISING CORRELATION MATRIX
##############################

# symnum(x, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
#        symbols = c(" ", ".", ",", "+", "*", "B"),
#        abbr.colnames = TRUE)
# 
# x: the correlation matrix to visualize
# cutpoints: correlation coefficient cutpoints. The correlation coefficients between 0 and 0.3 are replaced by a space (" "); correlation coefficients between 0.3 and 0.6 are replace by"."; etc .
# symbols : the symbols to use.
# abbr.colnames: logical value. If TRUE, colnames are abbreviated.

#replacing correlation coeficients by symbolds depending on level of correlation
symnum(data_cor, abbr.colnames = TRUE)


#Use corrplot() function: Draw a correlogram
install.packages("corrplot")
library(corrplot)
corrplot(data_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients. 
#In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.


#combine correlogram with the significance test. 
# Insignificant correlations are leaved blank - correlations with p-value > 0.01 are considered as insignificant.
corrplot(data_cor2$r, type="upper", order="hclust", 
         p.mat = data_cor2$P, sig.level = 0.01, insig = "blank")
#another display of same
corrplot(data_cor2$r, method="ellipse" ,type="upper", order="hclust",
         tl.col = "black", tl.srt = 45,
         p.mat = data_cor2$P, sig.level = 0.01, insig = "blank")


#Use chart.Correlation(): Draw scatter plots
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
mydata <- data[, c(4:19)]
chart.Correlation(mydata, histogram=TRUE, pch=19)
# In the above plot:
# The distribution of each variable is shown on the diagonal.
# On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
# On the top of the diagonal : the value of the correlation plus the significance level as stars
# Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", " ")


# co-relation heatmap 
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = data_cor, col = col, symm = TRUE)




#########################
# GGPLOT2 CORRELATION MATRIX HEATMAP
#########################
mydata <- data[, c(4:19)]
#compute correlation matrix
cormat <- round(cor(mydata),2)
head(cormat)
# create correlation heatmap w ggplot2
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#a correlation matrix has redundant information. We'll use the functions below to set half of it to NA
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
#library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
#library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# In the figure above :
# negative correlations are in blue color and positive correlations in red. The function scale_fill_gradient2 is used with the argument limit = c(-1,1) as correlation coefficients range from -1 to 1.
# coord_fixed() : this function ensures that one unit on the x-axis is the same length as one unit on the y-axis.


#Re-order correlation Matrix
# helper function
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


# Adding correlation coefficients
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

######################
# GGCORRPLOT
######################

#check: http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
#install.packages("ggcorrplot")
#library(ggcorrplot)
# Compute a correlation matrix
#data_cor <- data
#p.mat <- cor_pmat(data_cor)
