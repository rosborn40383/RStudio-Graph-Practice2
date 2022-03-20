#Installing the packages that will be necessary
library(plotly)
library(ggplot2)
library(corrplot)
library(datasets)
library(tidyverse)
library(psych)
library(tidyr)
#Reading in the dataset a "c". 
c<-read.csv("imports-85.data")
head(cc,5)
#Manually renaming all of the columns
names(c)[1]<-"symboling"
names(c)[2]<-"normalized_losses"
names(c)[3]<-"make"
names(c)[4]<-"fuel_type"
names(c)[5]<-"aspiration"
names(c)[6]<-"num_of_doors"
names(c)[7]<-"body_style"
names(c)[8]<-"drive_wheels"
names(c)[9]<-"engine_location"
names(c)[10]<-"wheel_base"
names(c)[11]<-"length"
names(c)[12]<-"width"
names(c)[13]<-"height"
names(c)[14]<-"curb_weight"
names(c)[15]<-"engine_type"
names(c)[16]<-"num_of_cylinders"
names(c)[17]<-"engine_size"
names(c)[18]<-"fuel_system"
names(c)[19]<-"bore"
names(c)[20]<-"stroke"
names(c)[21]<-"compression_ratio"
names(c)[22]<-"horsepower"
names(c)[23]<-"peak_rpm"
names(c)[24]<-"city_mpg"
names(c)[25]<-"highway_mpg"
names(c)[26]<-"price"
#Getting an idea for the dataset. Turned off skew because the information wasn't necessary for the data analysis.
describe(c,na.rm=TRUE,skew=FALSE)
#Checking what type of class each column is.
sapply(c,class)
#Creating a subset of the continuous variables.
cc<-subset(c, select = c("normalized_losses","wheel_base","horsepower","length","width","height","curb_weight","engine_size","bore","stroke","compression_ratio","peak_rpm","city_mpg","highway_mpg","price")) 
#Checking the correlation table for the continuous variables.
cor(cc,cc)
#Checking what is missing and then omiting the missing data from the sets.
is.na(cc)
cc%>%
  na.omit(cc)
c%>%
  na.omit(c)
sapply(cc,class)
#Changing all of the data classes to numeric to make them work in the following grpahs and tables.
cc[]<-lapply(cc, function(x) as.numeric(as.character(x)))
sapply(cc,class)
na.pass(cc)
#Confirming that the correlation table is complete and the missing data is not affecting it.
cor(cc, y = NULL, use = "na.or.complete")
#Naming another variable as the correlation matrix to use for heatmap
corr<- cor(cc, y = NULL, use = "na.or.complete")
#Creating a heat map. Also turning on the symmetry to give the best visualization.
heatmap(corr, symm=TRUE,)
#Making the continuous subset a data frame so that it can be plotted
as.data.frame(cc)
#Creating another subset with the categorical data.
categ<-subset(c, select = c("symboling","make","fuel_type","aspiration","num_of_doors","body_style","drive_wheels","engine_location","engine_type","num_of_cylinders","fuel_system"))
#Scatter Plot comparing length vs. width
ggplot(data=cc, mapping=aes(x=width, y=length, color=height)) +
  geom_point() + ggtitle("Width vs. Length (by height)")

#The distribution of the the continuous variables, with the missing data excluded.
ggplot(data=cc, mapping=aes(x=normalized_losses)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Normalized Losses')
ggplot(data=cc, mapping=aes(x=wheel_base)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Wheel Base')
ggplot(data=cc, mapping=aes(x=horsepower)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Horsepower')
ggplot(data=cc, mapping=aes(x=length)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Length')
ggplot(data=cc, mapping=aes(x=width)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Width')
ggplot(data=cc, mapping=aes(x=height)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Height')
ggplot(data=cc, mapping=aes(x=curb_weight)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Curb Weight')
ggplot(data=cc, mapping=aes(x=engine_size)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Engine Size')
ggplot(data=cc, mapping=aes(x=bore)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Bore')
ggplot(data=cc, mapping=aes(x=stroke)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Stroke')
ggplot(data=cc, mapping=aes(x=compression_ratio)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of compression Ration')
ggplot(data=cc, mapping=aes(x=peak_rpm)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Peak RPM')
ggplot(data=cc, mapping=aes(x=city_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of City MPG')
ggplot(data=cc, mapping=aes(x=highway_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Highway MPG')
ggplot(data=cc, mapping=aes(x=price)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Price')

#The distribution of the categorical data with the missing data excluded. stat="count" is used to to make the width of the bin the same size as the count.
ggplot(data=categ, mapping=aes(x=symboling)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Symboling')
ggplot(data=categ, mapping=aes(x=make)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Make')
ggplot(data=categ, mapping=aes(x=fuel_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel Type')
ggplot(data=categ, mapping=aes(x=aspiration)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Aspiration')
ggplot(data=categ, mapping=aes(x=num_of_doors)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Doors')
ggplot(data=categ, mapping=aes(x=body_style)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Body Style')       
ggplot(data=categ, mapping=aes(x=drive_wheels)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Drive Wheels')
ggplot(data=categ, mapping=aes(x=engine_location)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Enigne Location')
ggplot(data=categ, mapping=aes(x=engine_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Engine Type')
ggplot(data=categ, mapping=aes(x=num_of_cylinders)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Cylinders')
ggplot(data=categ, mapping=aes(x=fuel_system)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel System')

#Creating a pairplot of the continuous variables.

pairs(cc)


#Creating box plots comparing make to various variables.
ggplot(c, aes(x=make, y=price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=engine_size)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=normalized_losses)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=wheel_base)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=bore)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=stroke)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=compression_ratio)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=horsepower)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=peak_rpm)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=city_mpg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=highway_mpg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

