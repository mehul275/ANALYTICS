#K means clustering
install.packages("clue")
library(cluster)
library(caTools)
library(clue)
library(rfUtilities)

data = read.csv("Mall_customers.csv")
data = data[4:5]

train_set =  data[1:150,]    #giving only 75% data 
test_set =   data[151:200,]  #25% for testing

#elbow method : used to find optimum value of k
wcv = vector()    #empty vector which will contain total sum of sq..

for(i in 1:10)   #trying i=k=1-10 
{
  wcv[i] = sum( kmeans(train_set,i)$withinss )
} #sum of individual within cluster sum of squares for each cluster
 
plot(1:10,wcv,type = "b") #plotted all i=k for within cluster variation
                          #type = b means both points and lines
                          #k=6 is optimal value
#appying k means clustering
kmc = kmeans(train_set, centers = 5) #centers are no of k or centroids
clusters = kmc$cluster          #calling only cluster variable

#plotting clusters
clusplot(train_set,clusters,lines = F) #plot clusters from dataset : data  
                                       #and removing annoying lines 
#prediction
p=cl_predict(kmc,newdata=test_set)    #testing algo for new test_data
p

