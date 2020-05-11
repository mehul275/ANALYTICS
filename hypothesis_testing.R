#how to make a decision    based on p
#IF p<0.05 rejects Null Hypothesis 
#IF p>=0.05 rejects Alternate Hypothesis 


#1. implement a one-sample t-test    #tests actual vs estimated mean of one column
# Null Hypothesis : Mean of screensize sample does not differ from 10
#  Alternate Hypothesis : Mean screensize differ from 10
data <- read.csv('screen_size-data.csv')
View(data)

mean(data$Screen_size.in.cm.)       #the actual mean

qqnorm(data$Screen_size.in.cm.)       #plots the quantiles of data
qqline(data$Screen_size.in.cm., col='red')   # adds a line joining points

t.test(data$Screen_size.in.cm., mu = 10)     #mu is estimated mean


#2. implement a two-sample test      #tests difference of means bw two sample columns
data <- read.csv('ind.csv')
View(data)

var(data$screensize_sample1)    #variance of col 1
var(data$screensize_sample2)    #variance of col 2

# Null Hypothesis : if there is no difference b/w the mean of two samples
# Alternate Hypothesis : if there is difference b/w the mean of two samples

t.test(data$screensize_sample1, data$screensize_sample2, var.equal = T)


#3. paired sample t-test   #tests difference bw the means of same col but after some changes/modification/training
data <- read.csv('paired1.csv')
View(data)

# Null Hypothesis : if there is no difference b/w the mean of two samples
# Alternate Hypothesis : if there is difference b/w the mean of two samples

t.test(data$tyre_1, data$tyre_2, paired = T)

#rejects null hypo....







