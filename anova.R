#ANOVA
#anova is used to analyze the differences among group means in a sample.
data = read.csv("paired1.csv")

data$tyre_3 = data$tyre_2 + sample(200:500,25)  #adding 25 random no bw 200 and 500 

View(data)    

stacked_data = stack(data)    #combined into 1 col but with col names as groups names
View(stacked_data)            

anova = aov(values ~ ind,data = stacked_data)  #data as values and ind as group names
anova
summary(anova)

#f value is ratio of diff of means bw the grps and diff of means within the grp

#MANOVA
#multivariate analysis used to analyze data that involves more than one dependent variable at a time.
data = read.csv("paired1.csv")

data$tyre_3 = data$tyre_2 + sample(200:500,25)  #adding 25 random no bw 200 and 500 

maov = manova(cbind(tyre_1,tyre_2) ~  tyre_3,data = data)  #tyre_1,2 are independent var
summary(maov,test = "Pillai")      
summary(maov,test = "Roy")     
