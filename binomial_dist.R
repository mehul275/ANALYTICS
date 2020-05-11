#BINOMIAL DISTRIBUTION

#1 dbinom()  gives the probability distribution at each point.
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)
x
# Create the binomial distribution.
y <- dbinom(x,50,0.5)     # data , size , probability
y
# Plot the graph for this sample.
plot(x,y)


#2 pbinom() gives probability of random number to be less/more than  the value of a given number.
# Probability of getting 26 or less heads from a 51 tosses of a coin.
x <- pbinom(26,51,0.5,lower.tail = F)  
print(x)


#3 qbinom() gives quartiles from probability 
# How many heads will have a probability of 0.25 will come out when a coin is tossed 51 times.
x <- qbinom(0.25,51,1/2)
print(x)


#4. rbinom() generates required number of random values of given probability from a given sample.
# Find 8 random values from a sample of 150 with probability of 0.4.
x <- rbinom(8,150,0.4)
print(x)
