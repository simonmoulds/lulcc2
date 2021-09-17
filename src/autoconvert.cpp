#include <RcppArmadillo.h>
#include <numeric>
#include <Rcpp.h>
#include <unordered_set>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector autoconvert(IntegerVector lu, IntegerVector mask, NumericVector prob, IntegerVector codes) {

  int ncell = lu.size(), ncode = codes.size();
  IntegerVector alloc = clone(lu);

  // allocate automatic change
  for (int i = 0; i < ncell; i++) {
    int lu0 = lu[i];
    IntegerVector v = seq(0, ncode-1);
    LogicalVector l = (codes == lu0);
    IntegerVector a = v[l];
    if (a.size() != 1) {
      stop("error");
    }

    int ix = a[0] * ncell + i;

    // the trigger to make an automatic change is a suitability value of NA
    if (R_IsNA(prob[ix])) {

      double maxp = 0.0;
      for (int j = 0; j < ncode; j++) {
	int ixx = j * ncell + i;
	double pr = prob[ixx];
	if (!R_IsNA(pr)) {
	  int code = codes[j];
	  if (pr > maxp) {
	    maxp = pr;
	    alloc[i] = code;
	  }
	}
      }

      // if no other land use has a suitability > 0, 
      if (!maxp > 0) {
	alloc[i] = lu0;
      }	
      
    } else {
      
      if (mask[i] == 0) {
	alloc[i] = lu0;
      } else {
	alloc[i] = NA_INTEGER;
      }
    }
  }
  return alloc;
}

