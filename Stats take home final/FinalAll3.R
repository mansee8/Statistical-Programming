#Problem 1-Write random number generators for uniform and standard normal distributions. You
#can generate T numbers from each random number generator, and plot the histogram.

library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/Stats Submission/Stats take home final/Question1final.cpp')

hist(runifC(12345, 10000))

hist(rnormC(76543, 10000))

#Problem 2: Let (x) / expf????x2=(22)g with x being integers. Implement the Metropolis algo-
#rithm to sample from . You can start from X0 = 0, and then generate X1;X2; :::;XT . Plot the
#histogram of XB+1; :::;XT .


library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/Stats Submission/Stats take home final/Question1final.cpp')

seed=2345


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
    
    u2=u[2];
    accept=(u2<alpha(x,y))
    x=y*(accept==1)+x*(accept==0)
    
  }
  chain[n]=x
}
hist(chain,xlim=range(c(-30,30)),freq=FALSE)



# Problem 3

library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/Stats Submission/Stats take home final/Question1final.cpp')



gibbs<-function(T=1000, M=100, rho=0.99, x0=-10,y0=-10) {
  p<-rep(0,2*M*T)
  dim(p)<-c(2,T,M)
  
  for(m in (1:M))
  {
    x<-x0
    y<-y0
    p[,1,m]<-c(x,y)
    for(t in 2:T)
    {
      x<-rnorm(1,rho*y, sqrt(1-rho^2))
      y<-rnorm(1,rho*x, sqrt(1-rho^2))
      p[,t,m]<-c(x,y)
    }
  }
  p
}


#making the movie
install.packages("animation")
library(animation)
rho<-0.99
M=100
par(mar=c(2,2,1,2),mfrow=c(3,3))
bvn <- gibbs(x0=-10,y0=-10,M=M,rho=rho)
lims<-8*c(-1,1)
#plot.new()
#title(main="rho=0.5, x0=-10,y0=-10",xlab=100,ylab=100)

for(t in 1:10)
{
  plot(bvn[1,t,],bvn[2,t,],
       xlim=lims, ylim=lims,
       col=1:M,
       pch=16, main=paste('t=',t,'x0=-10,y0=-10,rho=0.5'))
  ani.pause(.2)
}

#saving as gif file 
#install.packages("ImageMagick")
saveGIF({
  for(t in 1:10){
    
    plot(bvn[1,t,],bvn[2,t,],xlim=lims,ylim=lims,col=1:M,pch=16,main=paste('t=',t))
  }
}, movie.name="animations_q3.gif"
)