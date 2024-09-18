// src/skewness_kurtosis.cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_calculate_skewness_kurtosis(NumericVector data) {
  int n = data.size();
  double mean = Rcpp::mean(data);
  double skewness = 0.0, kurtosis = 0.0;
  double sd = Rcpp::sd(data);

  for (int i = 0; i < n; i++) {
    skewness += pow((data[i] - mean) / sd, 3);
    kurtosis += pow((data[i] - mean) / sd, 4);
  }
  skewness = skewness / n;
  kurtosis = (kurtosis / n) - 3;

  return NumericVector::create(skewness, kurtosis);
}
