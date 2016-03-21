library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/Stats Submission/Stats take home final/Question1final.cpp')

seed=12345


# first we create a function program for f(x)
f=function(x,variance)
{
  exp(-x^2/(2*variance))
}
# let us define the acceptance function alpha(x,y) is
alpha=function(x,y,variance=2)
{
  min(1,f(y,variance)/f(y,variance))
}
# now we simulate the chain X0,X1,X2...Xt
N=10^3
A=10^2


chain=matrix(0,1,N)
for(n in 1:N){
  x=0;
  seed=seed*(n+2);
  for(i in 1:A)
  {
    u = runifC(seed * (i + 1), 2);
    u1=u[1];
    y=(x+1)*(u1>0.5)+(x-1)*(u1<=0.5)
   # r = ((r %*% 7621) + 1) %% 32768
    #outans2=c(runifC(sample(1:1000,1,replace=T)/n,1))
    u2=u[2];
    accept=(u2<alpha(x,y))
    x=y*(accept==1)+x*(accept==0)
    
  }
  chain[n]=x
}
hist(chain,xlim=range(c(-30,30)),freq=FALSE)

