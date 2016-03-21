library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/week 10 notes/Quetion3.cpp')



gibbs<-function(T=1000, M=100, rho=0.9, x0=0,y0=0) {
  p<-rep(0,2*M*T)
  dim(p)<-c(2,T,M)
  
  
  for(m in (1:M))
  {
    x<-x0
    y<-y0
    p[,1,m]<-c(x,y)
    for(t in 2:T)
    {
      #pi<-c(generateNormalNumber(2,sqrt(1-rho^2)))
      #x=pi[1]*(rho*x)+sqrt(1-rho^2);
      #y<-c(generateNormalNumber(,sqrt(1-rho^2)))
     # y=pi[2]*(rho*x)+sqrt(1-rho^2)
     # p[,t,m]<-c(x,y)
      x<-mynorm(1,rho*0, sqrt(1-rho^2))
      y<-mnorm(1,rho*x, sqrt(1-rho^2))
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

