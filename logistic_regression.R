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
                    #appying glm of bimoial/binary family on training data
prediction = predict(logistic_regression , type = 'response' , newdata = test_data)
View(prediction)
                      #type is range of answers   (0,1)
#rounding off answers to 0,1
prediction = ifelse(prediction>0.5 , 1 , 0)
View(prediction)            #if prediction > 0.5 then make it 1 otherwise leave

#marks/percentage of test
table(prediction)               #will count no of 0 and 1 only
matrix = table( test_data[,3], prediction)   #comparing y of test_data and prediction 
percentage=sum(matrix[1,1],matrix[2,2])/sum(matrix[1,1],matrix[2,2],matrix[1,2],matrix[2,1]) * 100
cat(percentage,"%",sep="")
