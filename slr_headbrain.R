library(ggplot2)
library(caTools)

hb=read.csv("headbrain.csv")
View(hb)

hb=hb[,c(3,4)]  #scling last 2 cols

#standardize
hb$Head.Size.cm.3. = scale(hb$Head.Size.cm.3.)
hb$Brain.Weight.grams. =scale(hb$Brain.Weight.grams.)
View(hb)

#train test split
split_hb = sample.split(hb$Brain.Weight.grams.,SplitRatio=0.75) 
split_hb   #randomly splitting the dependent var (y) into 75:25 coz. we have to hide 25% of y 
           #for which the model is developing to predict 

training_data = subset(hb,split_hb = T) #drawing out a subset where split_hb=T from dataset hb 
View(training_data)                     #which is to be shown to system

testing_data  = subset(hb,split_hb = F) #drawing out a subset where split_hb=F from dataset hb 
View(testing_data)                      #which is hidden from system

#applying linear_regression
linear_model = lm(formula = Brain.Weight.grams. ~ Head.Size.cm.3. , data = training_data)
  #we just give y and x variable's  values of only training_data (75%) for applying formula

train_prediction = predict(linear_model , newdata = training_data)
View(train_prediction)     #predicting training_data on which lm() model was trained

test_prediction = predict(linear_model, newdata = testing_data)
View(test_prediction)     #predicting test_data on which lm() model is developed

#plotting points of actual data and predicted best fit line
#1 for training_data
ggplot(training_data , aes(x=Head.Size.cm.3. , y=Brain.Weight.grams.)) + geom_point() + 
       geom_line(mapping = aes(x=Head.Size.cm.3. , y=train_prediction))

       #for plotting a point, we have taken actual x,y of training_data(75%)
       #for plotting a line, we have taken actual x and predicted y of train_prediction dataset 
          #generated after apppling lm()

#2 for testing_data
ggplot(testing_data, aes(x=Head.Size.cm.3. , y=Brain.Weight.grams.)) + geom_point() +
      geom_line(mapping = aes(x=Head.Size.cm.3. ,  y =test_prediction))
      #for plotting a point, we have taken actual x,y of training_data(25%)
      #for plotting a line, we have taken actual x and predicted y of test_prediction dataset 
      #generated after apppling lm()
      













