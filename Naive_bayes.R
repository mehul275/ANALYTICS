#NAIVES BAYES
install.packages("rfUtilities")
library(e1071)  #for calling algorithm
library(rfUtilities)  #for testing accuracy

data = read.csv("Social_Network_Ads.csv")

data = data[2:5]  #sliced 1st column : s.no
data$Purchased = factor(data$Purchased)  #making categorical data

split = sample.split(data,SplitRatio = 0.5)  #divinding into 60:40 
train_set = subset(data,split = T)   #giving only 60% data 
test_set = subset(data,split =F)     #40% for testing

#applying naives bayes
nb = naiveBayes(Purchased~. , data = train_set)  

#finding predictions
p = predict(nb , test_set)
View(p)
plot(p)
#testing accuracy
a = accuracy(p , test_set[,4]) #predicted vs actual
cat("ACCURACY = " , a$PCC , "%" , sep="" )  #printing in style

