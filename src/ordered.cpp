#include <RcppArmadillo.h>
#include <numeric>
#include <Rcpp.h>
#include <unordered_set>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector inC(IntegerVector x, IntegerVector table) {
  // C++ implementation of R base function '%in%'.
  // Adapted from https://github.com/asnr/advanced-r-answers/blob/master/performant_code/rcpp.md
  
  std::unordered_set<int> table_set;
  IntegerVector::iterator it;
  for (it = table.begin(); it != table.end(); it++) {
    table_set.insert(*it);
  }

  LogicalVector out(x.size());
  LogicalVector::iterator l_it;
  for (l_it = out.begin(), it = x.begin(); l_it != out.end(); l_it++, it++) {
    if (table_set.count(*it) > 0)
      *l_it = true;
    else
      *l_it = false;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector orderC(NumericVector x, bool decreasing) {
  // C++ implementation of R base function 'order'.
  // Adapted from: https://stackoverflow.com/questions/21609934/ordering-permutation-in-rcpp-i-e-baseorder
  
  IntegerVector res;
  if (decreasing) {
    res = wrap(arma::sort_index(as<arma::vec>(x), "descend"));
  } else {
    res = wrap(arma::sort_index(as<arma::vec>(x), "ascend"));
  }

  // res.attr("dim") = R_NilValue;
  return res;
}

// [[Rcpp::export]]
IntegerVector ordered(IntegerVector lu0, NumericMatrix tprob, IntegerVector dmd, IntegerVector alloc_order, IntegerVector categories, bool stochastic, int maxiter) {

  // lu0 is a vector containing land use values (non-NA only)
  // tprob is a matrix with total probability - make sure
  // neighbourhood/transition rules are taken into account already
  // dmd is a vector with the demand in terms of number of cells

  int ncat = categories.size();
  int ncell = lu0.size();
  IntegerVector lu1 = clone(lu0);
  NumericMatrix tprob1 = clone(tprob);

  // TODO: apply automatic transitions, mask values
  for (int i = 0; i < (ncat - 1); i++) {

    int cat = alloc_order[i];
    IntegerVector v = Rcpp::seq(0, ncat - 1);
    LogicalVector ix = (categories == cat);
    IntegerVector a = v[ix];
    
    if (a.size() != 1) {
      stop("error");
    }
    int cat_index = a[0];
    
    int d0 = std::count(lu1.begin(), lu1.end(), cat);
    int d1 = dmd[cat_index];
    int absdmd = abs(d1 - d0);
    
    // if (absdmd[cat_index] > 0) {
    if (absdmd > 0) {

      // bool incr = (dmd[cat_index] > 0);
      bool incr = (d1 > d0);

      IntegerVector v = Rcpp::seq(0, ncell - 1);
      IntegerVector chng_cat = alloc_order[Rcpp::seq(0, i)];

      // if lu has increasing demand, get the index of cells currently
      // belonging to other lus; if decreasing demand get index of cells
      // belonging to the lu
      LogicalVector cell_index_logical;
      if (incr) {
        cell_index_logical = !inC(lu1, chng_cat);
      } else {
	cell_index_logical = (lu1 == cat);
      }
      
      IntegerVector cell_index = v[cell_index_logical];
      
      // suitability of cells not currently belonging to current lu
      NumericVector pp = tprob1(_,cat_index);
      NumericVector p = pp[cell_index];

      IntegerVector order;
      if (incr) {
        order = orderC(p, true); 
      } else {
        order = orderC(p, false); 
      }

      p = p[order];
      IntegerVector cell_index2 = cell_index[order]; // index of cell as they appear in p
      
      int counter = 0;
      do {
        
        // continuously loop until demand is met
        int j = 0;
        do {
          
          if ((std::isfinite(p[j])) && (p[j] >= 0)) {
            bool test;
            if (stochastic) {
	      double mx = max(p);
              double rand = runif(1, 0, mx)[0];
              if (incr) {
                test = p[j] > rand;
              } else {
                test = p[j] < rand;
              }

            } else {
              test = true;
            }

            if (test) {
              int jj = cell_index2[j];
              if (incr) {
                lu1[jj] = cat;  // if increasing demand, change category
                tprob1(jj,_) = rep(-99.0, ncat);
		
              } else {
                lu1[jj] = -1;   // if decreasing demand, convert to holding category
              }

              absdmd -= 1;
              p[j] = -99.0;
              // tprob1(jj,_) = rep(-99.0, ncat);
            }
          }
          j++;
          
        } while ((absdmd > 0) && (j < p.size()));

        counter++;
        
      } while ((absdmd > 0) && (counter < maxiter));

      if (absdmd > 0) {
	Rprintf("Maximum number of iterations reached: demand not satisfied (%d)\n", absdmd);
      }
      
    }
  }

  // assign the last category to cells not already assigned higher
  // priority
  IntegerVector alloc_cat = alloc_order[Rcpp::seq(0, ncat-2)];
  LogicalVector cell_index_logical = !inC(lu1, alloc_cat);
  lu1[cell_index_logical] = alloc_order[ncat-1];
  return lu1;
}
  
