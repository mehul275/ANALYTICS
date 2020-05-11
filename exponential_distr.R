#EXPONENTIAL DISTRIBUTION

#1 dexp() gives the probability distribution at each point.
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,60,by = 1)
x
# Create the binomial distribution.
y <-  dexp(x, rate = 0.1) #rate israte of change of a quantity with respect to time
y
# Plot the graph for this sample.
plot(x,y)


#2 pexp() gives probability of random number to be less/more than  the value of a given number.
x <- pexp(2, rate = 0.9, lower.tail = TRUE)  #prob of 2 when rate is 0.9
print(x)


#3 qexp() gives quartiles from probability 
x <-qexp(0.2, rate = 0.1, lower.tail = TRUE)   #gives quantile data from prob=0.2 and rate as 0.1
print(x)


#4. rexp() generates required number of random values of given probability from a given sample.
x <- rexp(5, rate = 1)      #generates 5 random nos if rate = 1
print(x)
