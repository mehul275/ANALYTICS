#Hierarchical clustering
library(cluster)
library(caTools)
library(clue)

data = read.csv("Salary_Data.csv")

#handling missing values : filling not available  by averages....
data$YearsExperience = ifelse(is.na(data$YearsExperience) , ave(data$YearsExperience,
                       FUN=function(x) mean(x,na.rm = T) , data$YearsExperience)
                      
data$Salary =  ifelse(is.na(data$Salary) , ave(data$Salary,
                       FUN=function(x) mean(x,na.rm = T) , data$Salary)
            
#train test split          
train_data = data[1:7,]
test_data = data[8:10,]

#finding distance bw the data points
d = dist(train_data[],method = "euclidean")

#appyling hierarchical clustering
hca = hclust(d , method="complete")   #complete linkage method : max distance bw two points

#plotting hca
plot(hca)

#
p=hca.predict(hca,test_data)

























