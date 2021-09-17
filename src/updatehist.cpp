#include <numeric>
#include <Rcpp.h>
#include <unordered_set>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector updatehist(IntegerVector lu0, IntegerVector lu1, IntegerVector hist) {

  int ncell = lu0.size();
  for (int i = 0; i < ncell; i++) {
    if (lu0[i] == lu1[i]) {
      hist[i] += 1;
    } else {
      hist[i] = 0;
    }
  }

  return hist;

}

