library(ggplot2)
library(caTools)

aq=airquality
View(aq)

aq=aq[,c(3,4)]  #scaling last 2 cols

#standardize
aq$Wind = scale(aq$Wind)
aq$Temp =scale(aq$Temp)
View(aq)

#train test split
split_aq = sample.split(aq$Temp,SplitRatio=0.75) 
split_aq   #randomly splitting the dependent var (y) into 75:25 coz. we have to hide 25% of y 
           #for which the model is developing to predict 

training_data = subset(aq,split_aq = T) #drawing out a subset where split_aq=T from dataset hb 
View(training_data)                     #which is to be shown to system

testing_data  = subset(aq,split_aq = F) #drawing out a subset where split_aq=F from dataset hb 
View(testing_data)                      #which is hidden from system

#applying linear_regression
linear_model = lm(formula = Temp ~ Wind , data = training_data)
#we just give y and x variable's  values of only training_data (75%) for applying formula

train_prediction = predict(linear_model , newdata = training_data)
View(train_prediction)     #predicting training_data on which lm() model was trained

test_prediction = predict(linear_model, newdata = testing_data)
View(test_prediction)     #predicting test_data on which lm() model is developed

#plotting points of actual data and predicted best fit line
#1 for training_data
ggplot(training_data , aes(x=Wind , y=Temp)) + geom_point() + 
  geom_line(mapping = aes(x=Wind , y=train_prediction))

#for plotting a point, we have taken actual x,y of training_data(75%)
#for plotting a line, we have taken actual x and predicted y of train_prediction dataset 
#generated after apppling lm()

#2 for testing_data
ggplot(testing_data, aes(x=Wind , y=Temp)) + geom_point() +
  geom_line(mapping = aes(x=Wind ,  y =test_prediction))
#for plotting a point, we have taken actual x,y of training_data(25%)
#for plotting a line, we have taken actual x and predicted y of test_prediction dataset 
#generated after apppling lm()














