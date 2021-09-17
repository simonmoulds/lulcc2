#include <numeric>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector allowfun(IntegerVector lu, IntegerVector hist, IntegerVector codes, IntegerVector changedir, IntegerVector rules) {

  int ncell = lu.size(), ncode = codes.size();
  IntegerVector allow(ncell * ncode);
  IntegerVector tmp_allow(ncode);

  for (int i = 0; i < ncell; i++) {

    int lu0 = lu[i];
    if (!R_IsNA(lu0)) {

      IntegerVector v = seq(0, ncode-1);
      LogicalVector l = (codes == lu0);
      IntegerVector a = v[l];
      if (a.size() != 1) {
    	stop("error");
      }
      int lu0_ix = a[0];
      int lu_hist = hist[i];      
      
      for (int j = 0; j < ncode; j++) {
    	int ix = lu0_ix * ncode + j;
	
    	// rule 1: prohibit land use transitions
    	if (rules[ix] == 1) {
    	  tmp_allow[j] = 1;

    	} else if (rules[ix] == 0) {
    	  tmp_allow[j] = 0;
	  
    	} else if (rules[ix] == -1) { // transitions only allowed if demand increasing/decreasing

    	  if (changedir[lu0_ix] < 0) {
    	    tmp_allow[j] = 1;
    	  } else {
    	    tmp_allow[j] = 0;
    	  }
	  
    	} else if (rules[ix] == -2) { // land use can only change to code[j] if demand for code[j] increasing

    	  if (changedir[j] > 0) {
    	    tmp_allow[j] = 1;
    	  } else {
    	    tmp_allow[j] = 0;
    	  }

    	} else if ((rules[ix] >= 100) && (rules[ix] < 1000)) { // time restrictions

    	  if (lu_hist < (rules[ix] - 100)) {
    	    tmp_allow[j] = 1;
    	  } else {
    	    tmp_allow[j] = 0;
    	  }

    	} else if (rules[ix] > 1000) {

    	  if (lu_hist < (rules[ix] - 1000)) {
    	    tmp_allow[j] = 0;
    	  } else {
    	    tmp_allow[j] = 1;
    	  }

    	} else {
    	  tmp_allow[j] = 1;
      	}
      }

      for (int k = 0; k < ncode; k++) {
      	int ixx = k * ncell + i;
      	allow[ixx] = tmp_allow[k];
      }
    }
  }
  return allow;
}

