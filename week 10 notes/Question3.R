library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/week 10 notes/LevonQuestion1.cpp')



gibbs<-function(T=1000, M=100, rho=0.9, x0=1,y0=1) {
  p<-rep(0,2*M*T)
  dim(p)<-c(2,T,M)
  
  for(m in (1:M))
  {
    x<-x0
    y<-y0
    p[,1,m]<-c(x,y)
    for(t in 2:T)
    {
      #p<-c(generateNormalNumber(2,sqrt(1-rho^2)))
      #x=p[1]
      #y<-c(generateNormalNumber(,sqrt(1-rho^2)))
      #y=p[2]
      #p[,t,m]<-c(x+rho*y,y+rho*x)
      u<-rnormC(76543*t, 1)
      x<-(u[1]*rho*y)+sqrt(1-rho^2);
      y<-(u[2]*rho*x)+sqrt(1-rho^2);
      p[,t,m]<-c(x,y)
    }
  }
  p
}

#making the movie
install.packages("animation")
library(animation)
rho<-0.9
M=100
par(mar=c(2,2,1,2),mfrow=c(3,3))
bvn <- gibbs(x0=5,y0=-8,M=M,rho=rho)
lims<-8*c(-1,1)
for(t in 1:10)
{
  plot(bvn[1,t,],bvn[2,t,],
       xlim=lims, ylim=lims,
       col=1:M,
       pch=16, main=paste('t=',t))
  ani.pause(.2)
}

#saving as gif file 
install.packages("ImageMagick")
saveGIF({
  for(t in 1:10){
    plot(bvn[1,t,],bvn[2,t,],xlim=lims,ylim=lims,col=1:M,pch=16,main=paste('t=',t))
  }
}, movie.name=paste("bvn_gibbs_rho_",rho))

