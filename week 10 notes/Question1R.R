library(Rcpp)
sourceCpp('C:/Users/manse/Desktop/Books/statistical programming/week 10 notes/Quetion3.cpp')

outans=c(mynorm(3,2.5,2.55))
x=outans

hist(x,xlim = range(c(-1,1)),freq=F)
curve(dunif(x,0,1),add=T)

