  

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



myRidge <- function(X, Y, lambda)
{
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweep(A, p+1)
  beta = S[1:(p+1), p+2]
  return(beta)
}

n = 20
p = 500
sigma = .1
lambda = 1.

x = runif(n)
x = sort(x)
Y = x^2 + rnorm(n)*sigma
X = matrix(x, nrow=n)
for (k in (1:(p-1))/p)
  X = cbind(X, (x>k)*(x-k))
  
beta = myRidge(X, Y, lambda)
Yhat = cbind(rep(1, n), X)%*%beta

plot(x, Y, ylim = c(-.2, 1.2), col = "red")
par(new = TRUE)
plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")

