library(caTools)
library(ggplot2)

dataset=read.csv("Salary_Data.csv")
View(dataset)

options(scipen=1)

#handling missing values
dataset$YearsExperience = ifelse(is.na(dataset$YearsExperience),ave(dataset$YearsExperience,
                                FUN = function(x) mean(x,na.rm = T)), dataset$YearsExperience)

dataset$Salary = ifelse(is.na(dataset$Salary),ave(dataset$Salary,
                                FUN=function(x) mean(x,na.rm = T)), dataset$Salary)
View(dataset)
#standardization the values (-3,3)
dataset$YearsExperience = scale(dataset$YearsExperience)
dataset$Salary =  scale(dataset$Salary)

View(dataset)

#train test split
split_data = sample.split(dataset$Salary,SplitRatio = 0.75) 
split_data   #randomly splitting the data into 75% T and 25% F

training_data = subset(dataset, split_data = T)
View(training_data) #drawing out a subset where split_hb=T from dataset hb

testing_data =  subset(dataset, split_data = F)
View(testing_data)  #drawing out a subset where split_hb=F from dataset hb

#applying linear model using lm()
linear_model = lm(formula = Salary ~ YearsExperience,data = training_data) #y ~ x

train_prediction = predict(linear_model, newdata = training_data) #predicting training data
View(train_prediction)

test_prediction= predict(linear_model, newdata = testing_data) #predicting testing data
View(test_prediction)

#plotting points and line
ggplot(training_data, aes(x= YearsExperience, y=Salary)) + geom_point() + 
                    geom_line(mapping = aes(YearsExperience, train_prediction))

ggplot(testing_data, aes(x= YearsExperience, y=Salary)) + geom_point() + 
                   geom_line(mapping = aes(YearsExperience, test_prediction))
                                                 













