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
    
    Rcout<<"result "<<" = "<<out[i]<<endl;
    
  }
  return out;
  
}