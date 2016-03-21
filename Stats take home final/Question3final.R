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


