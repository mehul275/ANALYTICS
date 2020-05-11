#T DISTRIBUTION
  
#1 dt() gives the probability distribution at each point.
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,55,by = 1)
x
# Create the binomial distribution.
y <-  dt(x, df = 1) #rate is rate of change of a quantity with respect to time
y
# Plot the graph for this sample.
plot(x,y)


#2 pt() gives probability of random number to be less/more than  the value of a given number.
x <- pt(2, df = 1, lower.tail = TRUE)  #prob of 2 when rate is 0.9
print(x)


#3 qt() gives quartiles from probability 
x <- qt(0.2, df = 1, lower.tail = TRUE)   #gives quantile data from prob=0.2 and rate as 0.1
print(x)


#4. rt() generates required number of random values of given probability from a given sample.
x <- rt(5, df = 1)      #generates 5 random nos if rate = 1
print(x)
