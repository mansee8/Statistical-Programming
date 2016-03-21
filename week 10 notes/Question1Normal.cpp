#include <Rcpp.h>
#include <stdio.h>
#include <math.h>
using namespace std;
using namespace Rcpp;
// [[Rcpp::export]]

    
    
  double generateUniformNumber(double seed)
  {
    double uniformPrevResult = 1;
    double uniformResult = 0;
    double uniformIntPart = 0;
    
    double r = seed * uniformPrevResult;
    
    uniformResult = modf(r, &uniformIntPart); // To get rid of integer part
    
    uniformPrevResult = uniformResult;
    
    return uniformResult;
  }
  
  double igenerateNormalNumber(double seed)
  {
  double uniformSum = 0;
  
  for(unsigned i = 0; i < 12; ++i)
  {
    uniformSum += generateUniformNumber(seed);
  }
  
  double normalResult = uniformSum - 6;
  
  return normalResult; 
  }
  
  void generateGaussRandomDistribution(double r)
  {
  for(unsigned i = 0; i < 15; ++i)
  {
  r = ((r * 7621) + 1)/32768.0;
  double newNormalNumber =igenerateNormalNumber(r);
  
  cout<<"newNormalNumber = "<<newNormalNumber<<endl;
  }

  return ;
  }
