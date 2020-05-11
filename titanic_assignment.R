library(ggplot2)
library(dplyr)

z=read.csv("titanic_Data.csv")
View(z)

#What was the survival rate?

ggplot(z,aes(x=Survived)) + geom_bar(fill=c("green","red"),col="black") +labs(title="TITANIC DATA",y="rate") 
y=prop.table(table(z$Survived))*100
y
#What was the survival rate by gender? 

ggplot(z,aes(x=Survived,fill=Sex)) + geom_bar() + theme_bw() #theme_black_white makes a square boundary

#What was the survival rate by class of ticket? 

factor(z$Pclass)
ggplot(z,aes(x=Pclass,fill=Survived)) + geom_bar(fill=c("green","red","blue")) #showing only death rate 

#What was the survival rate by class of ticket and gender?   ??? t=3, g=2, s=2

ggplot(z,aes(x=Pclass,fill=Survived)) + geom_bar() +  facet_wrap(~Sex)
#bars of pclass, filling survived and visualized into two graphs 

#What is the distribution of passenger ages?  #how to visualize in bars by making categories????

ggplot(z,aes(x=Age)) + geom_boxplot()           

ggplot(z,aes(x=Age)) + geom_histogram(bins=10)

#What are the survival rates by age?  #how to draw bars for continous age

ggplot(z,aes(x=Age,fill=Survived)) + geom_boxplot() 


#after handling missing values
#doing ques 5 and 6 again

z$Age=ifelse(is.na(z$Age),ave(z$Age, FUN=function(x) mean(x,na.rm=T) ), z$Age)
View(z)

ggplot(z,aes(x=Age)) + geom_boxplot()           






































