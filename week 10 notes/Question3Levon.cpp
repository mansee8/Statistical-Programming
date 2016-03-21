#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <math.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;
/* Generate random uniform numbers using the linear congruential method */
// [[Rcpp::export]]
arma::vec runifC(double seed, int n) {
  
  /* This function takes in 2 inputs:
   seed: This is the seed used to generate the random numbers
   n: This is the number of random numbers we want to generate
   */
  double intpart;
  double x = seed; // Create a variable x of type double. Initialize its value to seed.
  arma::vec u(n);  // Create a vector u of length n. This is the vector of random uniform
  // numbers we will be generating
  
  for(int i = 0; i < n; ++i){                                      
    x = fmod(pow(7,5) * x, (pow(2,31) - 1.0));     // COMPLETE THIS LINE OF CODE
    u[i] = x / (pow(2,31) - 1.0);   // Normalize u[i] so that the values lie in [0, 1]
  }                                 // Remember that in C++, indexing starts at 0
  
  return(u); // This means we want our function to output the vector u.
  /* PLEASE NOTE THE FOLLOWING: we wrote "arma::vec" in front of
   our function name "runifC" on line 8. This is telling the program
   that our function is going to output something that is of type arma::vec.
   If I tried "return(x)", for example, we would have an error since 
   x is of type double. */
  
}
/*
// [[Rcpp::export]]
NumericVector UniformNumber(double A,int n) {
  
  
  double intpart;
  double prevResult = 1; //You can assign any value here
  double result;
  //double out[10];
  //static double resultans[10];
  NumericVector out(n);
  for(int i = 0; i < n; ++i)
  {
    double r = A * prevResult;
    
    result = modf(r, &intpart); // To get rid of integer part
    
    prevResult = result;
    out[i]=result;
    
    //Rcout<<"result "<<" = "<<out[i]<<endl;
    
  }
  return out;
  
}
*/
// [[Rcpp::export]]
arma::vec mynorm(int n,double mean,double sd) {
  
  arma::vec out(n);
  int i=1;
  int count=1;
  while(i<=n) {
    //generate 2 random uniform numbers and transform to unit square
    arma::vec n1=runifC(12345*i,2);
    //n1=UniformNumber(0.853,2);
    arma::vec n2;
    for(unsigned j = 0; j < n; ++j)
    { 
      n2[j] = 2*n1[j]-1;
    }
    
    double n3=0;
    int count=1;
    for(unsigned j = 0; j < n; ++j)
    { 
      n3 = n3+n2[j]*n2[j];
    }
    
    if(n3<=1) {
      double n4=sqrt(-2*log(n3)/n3);
      arma::vec n5(n);
      for(unsigned j = 0; j < n; ++j)
      { 
        n5 = n4*n2[j];
      }
      int p=count%(n+1);
      count=count+1;
      if(count%n==0)
        count=count+1;
      out[i]=sd*n5[p]+mean;
      i=i+1;
    }                                            
  }                                                 
  return out;
}                         