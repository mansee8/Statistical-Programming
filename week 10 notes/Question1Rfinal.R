library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/week 10 notes/Question1c.cpp')
#RandomNumberGenerator rndGen;
mansee=c(generateNormalNumber(1))
x=mansee
hist(x,xlim = range(c(-10,10)),freq=F)
#ist(x,col=gray(0.9),main="Normal on [0,1")
curve(dnorm(x),add=T)

outans=c(UniformNumber(10.234,10))
x=outans

hist(x,xlim = range(c(0,1)),freq=F)
curve(dunif(x,0,1),add=T)



