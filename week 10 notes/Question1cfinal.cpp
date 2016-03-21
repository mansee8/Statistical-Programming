#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;



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

// [[Rcpp::export]]
double igenerateNormalNumber(double seed,int n,double sd)
{
  double uniformSum = 0;
  
  NumericVector t=UniformNumber(seed,n);
  for(unsigned i = 0; i < n; ++i)
  { 
    uniformSum = uniformSum+ t[i];
  }
  
  double normalResult = uniformSum - sqrt(1-(sd*sd));
  Rcout<<normalResult;
  return normalResult; // 6 is a magic number
}


// [[Rcpp::export]]
NumericVector generateNormalNumber(int n,double sd)
{
  NumericVector a(n);
  double value;
  double r=0.253;
  
  
  for(int i=0;i<n;i++)
  {
    r = (0.12532*((double)i+1.0)) / 3;
    value =igenerateNormalNumber(r+i,n,sd);
    a[i]=value;
  } 
  //Rcout<<a;
  return a;
}