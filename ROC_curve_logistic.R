library(caTools)

dataset = read.csv("Social_Network_Ads.csv")  
View(dataset)

dataset = dataset[,3:5]   #taking age,salary as x and purchased as y
View(dataset)

# pre processing : 
#standardization
dataset$Age = scale(dataset$Age)    #scaling the values (-1,1)
dataset$EstimatedSalary= scale(dataset$EstimatedSalary)

View(dataset)
#train test split
split_data = sample.split(dataset, SplitRatio = 0.80)   #splitting 80:20
train_data = subset(dataset, split_data = T)  #training on T 
test_data = subset(dataset, split_data = F)   #testing on F

#applying logistic regression
logistic_regression = glm(formula = Purchased ~ . , family = binomial , data = train_data)
#appying glm of bimoial/binary family on training data, type will tell the range of predicted values 
log_prediction = predict(logistic_regression , type = 'response' , newdata = test_data)
prediction = ifelse(prediction>0.5 , 1 , 0)
View(prediction)                     #if prediction > 0.5 then make it 1 otherwise leave

#plotting ROC CURVE
install.packages("gplots")      #supports ROCR (required)
install.packages("ROCR")        #to implement ROC curve
library(gplots)
library(ROCR)    

ROC_predict = prediction(log_prediction,train_data$Purchased) #compare prediction with actual
ROC_perfor =  performance(ROC_predict,"tpr","fpr")   #plotting performance of algo
plot(ROC_perfor,colorize=T)            #plotting roc,coloring it, 
