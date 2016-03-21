norm_vec <- function(x) sqrt(sum(x^2))

gramschmidt_for_qr <- function(Aqr)
{
  # initializing Q and R matrix rows to 0
  Q <- matrix(0,ncol(Aqr),ncol(Aqr))
  R <- matrix(0,ncol(Aqr),ncol(Aqr))
  
  for (j in 1:ncol(Aqr))
  {
    v <- Aqr[,j]
    if (j > 1)
    {
      for (i in 1:(j-1))
      {
        R[i,j] <- t(Q[,i]) %*% Aqr[,j]
        v <- v - R[i,j] * Q[,i]
      }
    }
    
    R[j,j] <- norm_vec(v)
    Q[,j] <- v/R[j,j]
    
  }
  
  result <- list(Q, R)
  names(result) <- c("Q", "R")
  result
}






my_pca <- function(A)
{
  no_of_iterations = 1000
  n = nrow(A)
  V = matrix(rnorm(n*n), nrow = n)
  
  for (i in 1:no_of_iterations)
  {
    v_i = gramschmidt_for_qr(V)$Q
    V = A %*% v_i
    
  }
  
  
  B = gramschmidt_for_qr(V)
  B$Q = - B$Q
  result_pca <- list(B$Q, diag(B$R))
  names(result_pca) <- c("vectors","values")
  result_pca
}

X = iris[-5]
X = data.matrix(X)
means = colMeans(X)
for (i in 1:ncol(X))
{
  X[,i] = X[,i] - means[i]
}
A = t(X) %*% X / nrow(X)

ans = my_pca(A)

standard_deviation = sqrt(ans$values)
proportion_of_variance = ans$values/sum(ans$values)

print("STANDARD DEVIATION")
print(standard_deviation)

print("PROPORTION OF VARIANCE")
proportion_of_variance

irispca<-princomp(iris[-5])
summary(irispca)
