mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
myqr <- function(A)
{
  n = nrow(A)
  m = ncol(A)
  R = A
  Q = diag(n)
  
  for (k in 1:(m-1))
  {
    x = matrix(rep(0, n), nrow = n)
    x[k:n, 1] = R[k:n, k]
    g = sqrt(sum(x^2))
    v = x
    v[k] = x[k] + sign(x[k,1])*g
    
    s = sqrt(sum(v^2))
    if (s != 0)
    {
      u = v / s
      R = R - 2 * u %*% t(u) %*% R
      Q = Q - 2 * u %*% t(u) %*% Q
    }
  }
  result <- list(t(Q), R)
  names(result) <- c("Q", "R")
  result
}

mySweep <- function(A, m)
{
  n <- dim(A)[1]
  
  for (k in 1:m) 
  {
    for (i in 1:n)     
      for (j in 1:n)   
        if (i!=k  & j!=k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for (i in 1:n) 
          if (i!=k) 
            A[i,k] <- A[i,k]/A[k,k]  
          
          for (j in 1:n) 
            if (j!=k) 
              A[k,j] <- A[k,j]/A[k,k]  
            
            A[k,k] <- - 1/A[k,k] 
  }
  return(A)
}


mylm <- function(X, Y)
{
  #Z=(XY)T(XY)
  value_ii=t(X)%*%X
  value_ij=t(X)%*%Y
  value_ji=t(Y)%*%X
  value_jj=t(Y)%*%Y
  
  
  #row bind value_ii and value_ji
  rb_for_sweep_one=rbind(value_ii,value_ji)
  rb_for_sweep_two=rbind(value_ij,value_jj)
  
  #column bind rb_for_sweep_one and rb_for_sweep_two
  input_forsweep= cbind(rb_for_sweep_one,rb_for_sweep_two)
  
  #Applying sweep function
  ans=mySweep(input_forsweep,8)
  
  #Extracting Betahat
  beta_hat=ans[,c(9)]
  
  View(beta_hat)
  return(beta_hat)
}


myLogistic <- function(X, Y)
{
  n <- nrow(X)
  p <- ncol(X)    
  
  beta <- matrix(rep(0, p), nrow = p)
  epsilon <- 1e-6
  repeat
  {
    eta <- X%*%beta
    pr <- expit(eta)
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    Xwork <- mw*X
    Ywork <- sw*Z
    beta_new <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")$coefficient
    err <- sum(abs(beta_new-beta))
    beta <- beta_new
    if (err<epsilon)
      break
  }
  return(beta)
}

expit <- function(x)
{
  y <- 1/(1+exp(-x))
  return(y)
}

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
#mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

n = 400
p = 6
X = matrix(rnorm(n*p), nrow=n)
beta = matrix(rep(1, p), nrow = p)
Y = runif(n) < expit(X %*% beta)
myLogistic(X, Y)