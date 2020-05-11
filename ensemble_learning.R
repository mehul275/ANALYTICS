install.packages("xgboost")
library(xgboost)
library(caTools)
library(randomForest)
library(rfUtilities)

# pre processing
data=read.csv("Social_Network_Ads.csv")
data=data[3:5]
data$Purchased=factor(data$Purchased,levels=c(0,1))

split = sample.split(data,SplitRatio = 0.8)
train_set = subset(data,split = T)
test_set = subset(data,split = F)

train_set[-3]=scale(train_set[-3])
test_set[-3]=scale(test_set[-3])


# bagging for random forest classifier     algo(DT) , data = resampled subsets
  
rf=randomForest(train_set[-3],y=train_set$Purchased ,ntree=500 , importance=T)
names(rf)
imp=rf$importance
imp                          #fall in accurcay,gini index computed
rf

#predicting
p=predict(rf,test_set[-3]) #predicting y from x of test_data
View(p)

a=accuracy(p,test_set$Purchased)
a$PCC       #97.25%


# gradient boosting : convert weak to strong classifier by analysing misclassi...
dtrain =xgb.DMatrix(data=as.matrix(train_set[-3]),label=train_set$Purchased)
dtrain      #it need data in matrix form ,label is y, dmatrix makes normal matrix 
            #a sparse matrix : data=value otherwise 0....

dtest = xgb.DMatrix(data=as.matrix(test_set[-3]),label=test_set$Purchased)
dtest       #rest 20% data

watch_list = list(train=dtrain,test=dtest)
watch_list    #matrix to list

#appying gradient boosting classifier     algo(DT) , data = missclassified 
clf=xgb.train(data = dtrain,max.depth=8,watchlist = watch_list,nrounds=50)
  #takes a training data in matrix , nrounds = no of times new data is made 
#watchlist is weights to be taken from whole data to correct the missclassified rows

 