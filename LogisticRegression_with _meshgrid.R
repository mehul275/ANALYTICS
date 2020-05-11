library(caTools)

dataset <- read.csv('Social_Network_Ads.csv')
View(dataset)

dataset <- dataset[3:5]
View(dataset)


dataset$Purchased <- as.factor(dataset$Purchased)
str(dataset)

split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
train_split <- subset(dataset, split == T)
test_split <- subset(dataset, split == F)

train_split[-3] <- scale(train_split[-3])
test_split[-3] <- scale(test_split[-3])

View(train_split)

# GLM - Generalized Linear Model
classifier <- glm(formula = Purchased ~ . , 
                  family = binomial,
                  data = train_split)

prediction <- predict(classifier, type = 'response',
                      newdata = test_split)

View(prediction)
prediction <- ifelse(prediction > 0.5,  1,0)
View(prediction)

# confusion matrix
table(test_split[,3], prediction)


# Mesh grid
# install.packages("ElemStatLearn")
install.packages("ElemStatLearn")   #elements of stats learning
library(ElemStatLearn)

set <- train_split
# Age
x1 <- seq (min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)  #making age data more minute by 0.01 
View(x1)
# Salary
x2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01) #same for estimated salary
View(x2)

#combining data
grid_set <- expand.grid(x1,x2)       #making grid set by combining both the cols
View(grid_set)

colnames(grid_set) <- c('Age','EstimatedSalary') #setting their names

#applying logistic
predict_set <- predict(classifier,               #finding prediction of that grid_set data 
                    type = 'response',        
                    newdata = grid_set)
View(predict_set)                             #a very huge data

y_grid <- ifelse(prediction > 0.5, 1, 0)       #round off

#plotting
plot(set[,-3], main = "Logistic Prediction",  ##plotting y of training_data with x limit
     xlim = range(x1), ylim = range(x2))      #as x1 and y...

contour(x1,x2,matrix(as.numeric(y_grid), length(x1), 
                     length(x2)), add = T)    #plotting x1,x2 on mesh of matrix  

points(grid_set, pch = '.', 
       col = ifelse(y_grid == 1, 'springgreen', 'tomato'))

points(set, pch = 21, bg = ifelse(set[,3] == 1, 
                                  'green4', 'red3'))








