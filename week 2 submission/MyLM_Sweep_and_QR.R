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

mylm_sweep <- function(X, Y)
{
  X = cbind(a = 1,X)
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
  
  return(beta_hat)
}

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


my_Solve <- function(X,Y)
{
  a = nrow(X)
  m = ncol(X)
  x_vector <- cbind(X,Y)
  y_vector <- cbind(t(Y),1)
  Z <- rbind(x_vector,y_vector)
  S = mySweep(Z,m)
  print(ncol(S))
  inverse = S[,(m+1)]
  return(inverse)
}

mylm_qr <- function(X,Y)
{
  #adding a weighted column of 1s to perform AX=B
  X1 = cbind(a = 1,X)
  p = 7
  final = cbind(X1,Y)
  R = myqr(final)$R
  R1 = R[1:(p+1), 1:(p+1)]
  Y1 = R[1:(p+1), p+2]
  beta = my_Solve(R1,Y1)
  return(beta)

}

state.x77                            
str(state.x77)                     
state_matrix = as.data.frame(state.x77) 
row.names(state_matrix)=NULL
str(state_matrix)
MatrixA=data.matrix(state_matrix)

# X such that life expectency is removed

X=MatrixA[,-c(4)]
Y=MatrixA[,c(4)]
print("Betahat using mylm_sweep")
beta_hat_ans=mylm_sweep(X,Y)
beta_hat_ans

print("Betahat using mylm_QR")
ans_final=mylm_qr(X,Y)
ans_final

print("Regression results using lm(Y~X)")
lm(Y~X)



