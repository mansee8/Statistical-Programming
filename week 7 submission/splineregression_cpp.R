library(Rcpp)

sourceCpp('mySweep_new_cpp.cpp')

myRidgeCpp <- function(X, Y, lamda)
{
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lamda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweep_cpp(A, p+1)
  beta = S[1:(p+1), p+2]
  return(beta)
}

ridge_func <- function() {
  
  n = 100
  p = 500
  sigma = 0.1
  lamda = 1.
  
  x = runif(n)
  Y = x^2 + rnorm(n)*sigma
  X = matrix(x, nrow=n)
  
  beta = myRidgeCpp(X, Y, lambda)
  Yhat = cbind(rep(1, n), X)%*%beta
  
  plot(x, Y, ylim = c(-0.2, 1.2), col = "red")
  par(new = TRUE)
  plot(x, Yhat, ylim = c(-0.2, 1.2), col = "green")
  
}

spline_func <- function()
{
  n = 100
  p = 500
  num_train = 70
  num_test = 30
  sigma =0.1
  x = sort(runif(n))
  Y = x^2 + rnorm(n)*sigma
  

  lamda <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  train_error <- rep(0,7)
  test_error <- rep(0,7)
  
  train_data = x[1:70]
  test_data = x[71:100]
  train_label = Y[1:70]
  test_label = Y[71:100]


  it = 0
  for (value in lamda)
  {
    it = it + 1
    trainX = matrix(train_data, nrow=num_train)
    testX = matrix(test_data, nrow = num_test)
    for (k in (1:(p-1))/p)
    {
      trainX = cbind(trainX, (train_data>k)*(train_data-k))
      testX = cbind(testX, (test_data>k)*(test_data-k))
    }
  
    beta = myRidgeCpp(trainX, train_label, value)
    trainYhat = cbind(rep(1, num_train), trainX)%*%beta
    testYhat = cbind(rep(1, num_test), testX)%*%beta
  

    plot(train_data, train_label, ylim = c(-0.2, 1.2))
    par(new = TRUE)
    plot(train_data, trainYhat, ylim = c(-0.2, 1.2), type = 'l')
  
    train_error[it] <- (sum((trainYhat - train_label)^2)/num_train)
    test_error[it] <- (sum((testYhat - test_label)^2)/num_test)
  
  }

  plot(log(lamda), train_error,type = 'l')
  plot(log(lamda), test_error,type = 'l')

}


ridge_func()


spline_func()

