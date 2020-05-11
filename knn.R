install.packages("e1071")    

library(caret) # for Classification And Regression Training
library(e1071) # for latent class analysis

data = read.csv("US Presidential Data.csv")

data$Win.Loss = as.factor(data$Win.Loss)    #making categorical data 
View(data)

#sets a fixed pattern of random data ; same for all iterations
set.seed(101)

#training 80% of the data
split_data = sample.split(data, SplitRatio = 0.80)   #splitting 80:20
train_data = subset(data, split_data = T)  #training on T 
test_data = subset(data, split_data = F)   #testing on F

#making names from levels/fcators of Win.Loss col
levels(train_data$Win.Loss) = make.names(levels(as.factor(train_data$Win.Loss)))
levels(test_data$Win.Loss) = make.names(levels(as.factor(test_data$Win.Loss)))
     #levels named as x0,x1 made names of classifier 

#making train control
y = trainControl(method ="repeatedcv",repeats = 4,number = 3,classProbs = TRUE,summaryFunction = twoClassSummary)
     #repeatedcv is called for resampling,  repeats is no of resampling iterations : 8 times reshuffled
     #numbers is no of subsets of data to be made,  classProbs is probability of predicted values,  
     #summaryFunction shows the value of ROC if not used shows kappa
y
#applying knn
knn <- train(Win.Loss ~ ., data = train_data, method = "knn",preProcess = c("center","scale"),
               trControl = y, metric = 'ROC', tuneLength = 10)
    #method  is algo, pre process takes 0 as centre and then scale ,trcontrol is how the func should act, 
    #metric is to select the optimal model based on which error, tune length is no of k to be tried
plot(knn)

#predicting test_data
p=predict(knn,test_data)
View(p)
q=predict(knn,test_data,type = "prob")   #probabilities of predicted values
View(q)
