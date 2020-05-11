#BERNOULLI DISTRIBUTION
install.packages("Rlob")
library(Rlob)

#1 dbern()  gives density of uniform distribution
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by =1)
x
# Create the poisson distribution.
y <- dbern(x, 0.4)     
y
# Plot the graph for this sample.
plot(x,y)


#2 pbern() gives probability of random numbers.
x <- pbern(q, prob=.5, lower.tail=TRUE)
print(x)


#3 qbern() gives quartiles from probability 
x <- qbern(p, prob=.5, lower.tail=TRUE)
print(x)


#4. rbern() generates required number of random values of given probability from a given sample.
x <- rbern(n, prob=.5)
print(x)



