#DECISION TREE
install.packages("party")  
install.packages("rpart")
install.packages("rpart.plot")

library(party)
library(rpart)    #for training algo
library(rfUtilities)
library(rpart.plot)  #for plotting dt

rs = readingSkills   #precefined file
View(rs)

rs$nativeSpeaker = as.numeric(rs$nativeSpeaker) - 1
View(rs$nativeSpeaker)   #making yes = 1 and no = 0

split = sample.split(rs,SplitRatio = 0.8)  #divinding into 80:20 
train_set = subset(rs,split = T)   #giving only 60% data 
test_set = subset(rs,split =F)     #40% for testing

#applying decison tree algo
dt = rpart(nativeSpeaker~. , data=train_set )

#predicting ....
p = predict(dt , newdata = test_set)
View(p)

#Accuracies
p = ifelse(p>0.5 , 1,0)

a = accuracy(p , test_set[,1]) #predicted vs actual
cat("ACCURACY = " , a$PCC , "%" , sep="" )  #printing in style

#plotting decsion tree
rpart.plot (dt)  #drawing beautiful decision tree




















