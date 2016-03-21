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
NumericVector mynorm(int n,double mean,double sd) {

NumericVector out(n);
int i=1;
int count=1;
while(i<=n) {
//generate 2 random uniform numbers and transform to unit square
NumericVector n1=UniformNumber(0.853,2);
    //n1=UniformNumber(0.853,2);
    NumericVector n2;
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
      NumericVector n5(n);
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
