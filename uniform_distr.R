#UNIFORM DISTRIBUTION

#1 dunif()  gives density of uniform distribution
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by =1)
x
# Create the uniform distribution.
y <- dunif(x, min = 10, max = 40)     
y
# Plot the graph for this sample.
plot(x,y)


#2 pbinom() gives probability of random numbers.
x <- punif(26, min = 10, max = 40, lower.tail = F)
print(x)


#3 qbinom() gives quartiles from probability 
x <- qunif(0.1, min = 10, max = 40, lower.tail = TRUE)
print(x)


#4. rbinom() generates required number of random values of given probability from a given sample.
x <- runif(100, min = 10, max = 40)
print(x)


