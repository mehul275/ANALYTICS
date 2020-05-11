#POISSON DISTRIBUTION

#1 dpois()  gives density of uniform distribution
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by =1)
x
# Create the poisson distribution.
y <- dpois(x, lambda=7)     
y
# Plot the graph for this sample.
plot(x,y)


#2 ppois() gives probability of random numbers.
x <- ppois(6, lambda=7, lower.tail = F)
print(x)


#3 qpois() gives quartiles from probability 
x <- qpois(0.3,lambda = 7, lower.tail = TRUE)
print(x)


#4. rpois() generates required number of random values of given probability from a given sample.
x <- rpois(100, lambda = 6)
print(x)


