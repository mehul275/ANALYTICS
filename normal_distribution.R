#NORMAL DISTRIBUTION

#1. dnorm()  gives the height of prob. distribution  
# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = 0.1)
x
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5)     #
y
#plotting
plot(x,y)


#2. pnorm() gives probability of random number to be less/more than  the value of a given number.  
# Create a sequence of numbers between -10 and 10 incrementing by 0.2.
x <- seq(-10,10,by = .2)
x
# Choose the mean as 2.5 and standard deviation as 2. 
y <- pnorm(x, mean = 2.5, sd = 2,lower.tail = T)
y
# Plot the graph.
plot(x,y)


# 3. qnorm()  gives quartiles from probability
# Create a sequence of probability values incrementing by 0.02.
x <- seq(0, 1, by = 0.02)
x
# Choose the mean as 2 and standard deviation as 3.
y <- qnorm(x, mean = 2, sd = 1)
y
#Plot the graph.
plot(x,y)


# 4. rnorm() generates random numbers whose distribution is normal.
# Create a sample of 50 numbers which are normally distributed.
y <- rnorm(40)
y
# Plot the histogram for this sample.
hist(y, main = "Normal DIstribution")
