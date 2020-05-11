View(mtcars)

attach(mtcars)   #attach is used to attach the cols to program to use it directly
plot(disp,mpg)

linear_model <- lm(mpg ~ disp, data = mtcars)
coef(linear_model)

y_pred <- predict(linear_model)
abline(linear_model)   #abline is inbulit fun

n <- length(disp)  #
threshold <- 0.01   #target
epochs <- 50000   #iteration
alpha <- 0.002    #constant used to decrease error uniformly

gradientDescent <- function(x,y,alpha,n,epochs)#x/y are indepen/depend var  
{                                             #n is length of displacement col
  m <- runif(n = 1, min = 0, max = 1)         #random unif distr  bw 0 and 1 
  c <- runif(n = 1, min = 0, max = 1)
  y_pred <- m * x + c                         #randon line
  mse <- sum((y - y_pred) ^ 2) / n            #formala
  converged = F                               #for testing we have reached threshold or not
  i = 0                                       #counts no of iterations
    while(converged == F) 
      {                         
        grad_m <- ((1/n) * (sum((y_pred - y) * x)))    #differe wrt m
        m <- m - alpha * grad_m                       #multiply by a constant
        grad_c <- ((1/n) * (sum((y_pred - y))))        #diff wrt c
        c <- c - alpha * grad_c
        
        y_pred <- m * x + c                               #new y predict by using new m and c
        mse_new <- sum((y - y_pred) ^ 2) / n             #again finding new error
        
        if(mse - mse_new <= threshold)
        {                 #1st case   #finding the diff of error if == 0.01
          print("Error Optimized")                       #then print ""
          abline(c,m)                                    #darwing a new optimised line
          converged = T                                 #to get out of the loop
          return(paste("Intercept :",c,"Slope : ",m,"Error : ",mse_new))#returning outputs
        }
  
        i = i + 1                                          #counting iterations i
        if(i > epochs)
        {                               #2nd case    #stop at epochs +1
          print("Epcohs Completed")
          abline(c,m)
          converged = T
          return(paste("Intercept :",c,"Slope : ",m,"Error : ",mse_new))
        }
      }
}

gradientDescent(disp, mpg, alpha, n, epochs)           #calling function

