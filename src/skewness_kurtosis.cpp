#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_calculate_skewness_kurtosis(NumericVector data) {
  int n = data.size();
  double mean = Rcpp::mean(data);
  double sd = Rcpp::sd(data);

  double skewness = 0.0, kurtosis = 0.0;

  // ACF Improve this algorithm by removing the elements that do not change with i
  for (int i = 0; i < n; i++) {
    skewness += pow((data[i] - mean) / sd, 3);
    kurtosis += pow((data[i] - mean) / sd, 4);
  }
  skewness = skewness / n;
  kurtosis = (kurtosis / n);

  return NumericVector::create(skewness, kurtosis);
}
