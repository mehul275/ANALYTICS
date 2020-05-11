#multi linear regression  
library(caTools)        #for plotting lines,points
library(corrplot)       #for choosing color, text labels, color labels, layout, etc.
library(caret)          #for data splitting, pre processing

dataset = read.csv("appl_1980_2014.csv")
View(dataset)

dataset = dataset[,2:6]   #taking open,high,low,vol and close cols
View(dataset)

split = sample.split(dataset, SplitRatio = 0.80)  #splitting the data into 80:20
train_set = subset(dataset, split = T)            #training 80% of dataset
test_set = subset(dataset, split = F)             #testing on 20%

linear_model =lm(formula = Close ~ . , data=train_set)   # . means all cols

prediction = predict(linear_model , newdata = test_set)    #prediction on test_set 
View(prediction)

summary(linear_model)$coeff
summary(linear_model)$r.squared               #accuracy percentage = 99.99%
summary(linear_model)$adj.r.squared           #coz it is only for 80% of the data
#####################################################################################################

#dimensionality reduction
dataset_x = dataset[,-c(4)]     #making group of independent var
View(dataset_x)

#finding correlation matrix
cor_matrix = cor(dataset_x)     #correlation matrix
cor_matrix

#plotting correlation plot         #number is showing degree of correlation 
corrplot(cor_matrix,method = "number")     

#finding correlation and removing the highly correlated cols
cor_index = findCorrelation(cor_matrix,cutoff = 0.7)
cor_index                   #it will return 2,1 only as 1,2,3 is coverted into 3 only 

dataset_x = dataset_x[,-c(cor_index)]  #removing highly corr cols 
View(dataset_x)

#adding Close in dataset_x 
dataset = read.csv("appl_1980_2014.csv")  #again reading the file to get close col
View(dataset)
dataset_x$Close = dataset$Close              #adding close col again 
View(dataset_x)

#appyling linear model after dimensionality reduction
linear_model = lm(formula = Close ~ . , data = dataset_x)

prediction = predict(linear_model, newdata  = dataset_x)    #finding predictions based 
View(prediction)                                            #upon 2 cols only(least correlated)

#finding errros
summary(linear_model)$coeff
summary(linear_model)$r.squared               #accuracy percentage = 99.98%
summary(linear_model)$adj.r.squared           #accuracy percentage = 99.98%

# calculating RSquared Manually
y = dataset_x$Close
r_squared <- 1 - sum((y - prediction) ^ 2) / sum((y - mean(y)) ^ 2)
print(r_squared)                              #accuracy percentage = 99.98%

# calculating RMSE
rmse <- sqrt(mean((dataset$Close - prediction) ^ 2))
print(rmse)    #it should be close to 0                  #1.759







