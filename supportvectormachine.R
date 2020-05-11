#support vector machine
library(e1071)
library(rfUtilities)
data = read.csv("Social_Network_Ads.csv")
data = data[3:5]
View(data)

#train_test_split
split = sample.split(data,SplitRatio = 0.8)
train_set = subset(data,split = T)
test_data = subset(data,split = F)

#applying linear and radial/kernel svm
svm = svm(Purchased~. , train_set , type="C-classification" , kernel='linear')
plot(x=svm,data=train_set)

svm = svm(Purchased~. , train_set , type="C-classification" , kernel='radial')
plot(x=svm,data=train_set)

#predicting
p = predict(svm,test_data[,-3])
View(p)

#accuracy
a=accuracy(p,test_data[,3])
cat(a$PCC,"%")
