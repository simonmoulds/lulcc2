/* Author: Simon Moulds
   Date: 14/07/2014
   Version: 1.0
   Licence: GPL v3 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

double *computeChange(double *lu, double *pastlu, double *potential, double *elasticity, double *minchange, double *maxchange, double *changedir, double *changerule, double *minvalue, double *maxvalue, int ncell, int ncode) {

  int i, j, ix;
  double pot, change;
 
  for (i = 0; i < ncode; i++) {    
    for (j = 0; j < ncell; j++) {
      ix = i * ncell + j;
      pot = potential[ix];
      change = pot * elasticity[i];

      if (fabs(change) < minchange[i]) {
	pot = 0;
	change = 0;
      }

      if (fabs(change) >= maxchange[i]) {
	change = maxchange[i] * (pot / fabs(pot));
      }

      if (((pot >= 0) && (changedir[i] == 1) && (changerule[i] < 1)) || ((pot <= 0) && (changedir[i] == -1) && (changerule[i] < 1))) {
	lu[ix] = pastlu[ix] + change;
      } else if (changerule[i] != 0) {
	lu[ix] = pastlu[ix];
      }

      if (lu[ix] < 0) {
	lu[ix] = 0;
      } else if (lu[ix] > 1) {
	lu[ix] = 1;
      }

      if (lu[ix] <= minvalue[i]) {
	if (pastlu[ix] >= minvalue[i]) {
	  lu[ix] = minvalue[i];
	} else {
	  lu[ix] = pastlu[ix];
	}
      }

      if (lu[ix] > maxvalue[i]) {
	if (pastlu[ix] <= maxvalue[i]) {
	  lu[ix] = maxvalue[i];
	} else {
	  lu[ix] = pastlu[ix];
	}
      }
    }
  }
  
  return lu;
    
}

double *correctCellChange(double *lu, double *pastlu, double *changedir, double *changerule, double *minvalue, double *maxvalue, int ncell, int ncode) {

  int i, j, ix, counter;
  double nostatic, decr, incr, totcover, totchange, totstatic, diff, amin, amax, max, aux, ludirect, lustatic; 

  double backp = 0.5;

  for (j = 0; j < ncell; j++) {

    nostatic = 0;
    decr = 0;
    incr = 0;
    totcover = 0;
    totchange = 0;
    totstatic = 0;
    amin = lu[j] - pastlu[j]; 	/* first land use type */
    amax = amin;
    max = fabs(amax);
    counter = 0;
    
    for (i = 0; i < ncode; i++) {
      ix = i * ncell + j;
      totcover = totcover + lu[ix];
    } /* OK */

    if (fabs(totcover - 1) > 0.005) {
      for (i = 0; i < ncode; i++) {
	ix = i * ncell + j;
	diff = lu[ix] - pastlu[ix];
	totchange = totchange + abs(diff);

	if (abs(diff) > max) {
	  max = abs(diff);
	}

	if (diff > amax) {
	  amax = diff;
	}

	if (diff < amin) {
	  amin = diff;
	}
      }
    } /* OK */

    /* "adapts land use/land cover types if all of them change in the same direction" */
    if (totchange > 0) {
      if ((backp * totchange) > (max * 0.5)) {
	backp = (max / (2 * totchange));
      }
    } /* OK */

    for (i = 0; i < ncode; i++) {
      ix = i * ncell + j;
      lustatic = changerule[i];

      if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	lustatic = 1;
      }

      if (lustatic < 1) {
	diff = lu[ix] - pastlu[ix];

	if (diff > (-1 * backp * totchange)) {
	  incr = incr + 1;
	}

	if (diff < (backp * totchange)) {
	  decr = decr + 1;
	}

      } else {
	nostatic = nostatic + 1;
      }
    } /* OK */

    /* check if all lu types (except those that are static) are decreasing or increasing */
    if ((decr == (ncode - nostatic)) || (incr == (ncode - nostatic))) {
      for (i = 0; i < ncode; i++) {
	ix = i * ncell + j;
	ludirect = changedir[i];  /* change direction according to demand */
	lustatic = changerule[i];

	if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	  lustatic = 1;
	}

	if (lustatic < 1) {

	  if (incr == (ncode - nostatic)) {
	    lu[ix] = lu[ix] - (amin + (backp * totchange));
	  }

	  if (decr == (ncode - nostatic)) {
	    lu[ix] = lu[ix] + ((backp * totchange) - amax);
	  }

	  if (lu[ix] < 0) {
	    lu[ix] = 0;
	  }
	}

	if ((ludirect == 1) && (lu[ix] < pastlu[ix]) && (changerule[i] == -1)) {
	  lu[ix] = pastlu[ix];
	}

	if ((ludirect == -1) && (lu[ix] > pastlu[ix]) && (changerule[i] == -1)) {
	  lu[ix] = pastlu[ix];
	}
      }
    } /* OK */

    /* "perform corrections" */
    do {
      
      counter = counter + 1;
      totcover = 0;
      totchange = 0;
      totstatic = 0;
      /* Rprintf("%d\n", counter); */

      for (i = 0; i < ncode; i++) {
	ix = i * ncell + j;
	lustatic = changerule[i];

	if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	  lustatic = 1;
	}

	if (lustatic < 1) {
	  totcover = totcover + lu[ix];
	} else {
	  totstatic = totstatic + lu[ix];
	}

	totchange = totchange + fabs(lu[ix] - pastlu[ix]);
      }

      if (fabs(totcover - (1 - totstatic)) > 0.005) {

	if (totchange == 0) {

	  for (i = 0; i < ncode; i++) {
	    ix = i * ncell + j;
	    lustatic = changerule[i];

	    if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	      lustatic = 1;
	    }

	    if (lustatic < 1) {
	      lu[ix] = lu[ix] * ((1 - totstatic) / totcover);
	    }
	  }

	} else {

	  for (i = 0; i < ncode; i++) {
	    ix = i * ncell + j;
	    aux = lu[ix];
	    lu[ix] = lu[ix] - (fabs(lu[ix] - pastlu[ix]) * ((totcover - (1 - totstatic)) / totchange));

	    if (lu[ix] < 0) {
	      lu[ix] = 0;
	    }
	  }
	}
      }

    } while (fabs(totcover - (1 - totstatic)) > 0.005 && counter < 25);
    
    if (counter >= 25) {
      totcover = 0;
      totstatic = 0;

      for (i = 0; i < ncode; i++) {
	ix = i * ncell + j;
	lustatic = changerule[i];
	
	if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	  lustatic = 1;
	}

	if (lustatic < 1) {
	  totcover = totcover + lu[ix];
	} else {
	  totstatic = totstatic + lu[ix];
	}
      }

      for (i = 0; i < ncode; i++) {
	ix = i * ncell + j;
	lustatic = changerule[i];

	if ((lu[ix] <= minvalue[i]) || (lu[ix] >= maxvalue[i])) {
	  lustatic = 1;
	}

	if (lustatic < 1) {
	  lu[ix] = lu[ix] * ((1 - totstatic) / totcover);
	}
      }
    }
  }

  return lu;

}

double *calculateArea(double *area, double *lu, double cellarea, int ncell, int ncode) {
  /* function to calculate total area belonging to each land use type */
  
  int i, j, ix;
  /* double area [ ncode ]; */

  for (i = 0; i < ncode; i++) {
    double ar = 0;
    for (j = 0; j < ncell; j++) {
      ix = i * ncell + j;
      if (lu[ix] > 0) {
	ar = ar + lu[ix];
      }
    }
    area[i] = ar * cellarea;
  }
  
  return area;

}
 
SEXP allocateclue(SEXP r, SEXP lu0, SEXP d, SEXP elas, SEXP cr, SEXP carea, SEXP minelas, SEXP maxelas, SEXP minc, SEXP maxc, SEXP minv, SEXP maxv, SEXP mi, SEXP md, SEXP ncl, SEXP ncd) {

    R_len_t i, j;
    int count, ix, flag, allocation_ok, flagflex;
    double *regr, *landuse0, *landuse1, *demand, *elasticity, *potential, *changedir, *changerule, *minchange, *maxchange, *minvalue, *maxvalue, *area0, *area1, ludirect, cd, diff, maxadjust, tmpmaxdiff;

    /* int ncell = length(lu); */
    /* int ncode = length(d); */

    PROTECT(r = coerceVector(r, REALSXP)); 
    regr = REAL(r);

    PROTECT(lu0 = coerceVector(lu0, REALSXP));
    landuse0 = REAL(lu0);
   
    PROTECT(d = coerceVector(d, REALSXP));
    demand = REAL(d);

    /* PROTECT(elas = coerceVector(elas, REALSXP)); */
    /* elasticity = REAL(elas); */
    SEXP elas_copy = PROTECT(coerceVector(duplicate(elas), REALSXP));
    elasticity = REAL(elas_copy);
    

    PROTECT(cr = coerceVector(cr, REALSXP));
    changerule = REAL(cr);

    PROTECT(minc = coerceVector(minc, REALSXP));
    minchange = REAL(minc);

    PROTECT(maxc = coerceVector(maxc, REALSXP));
    maxchange = REAL(maxc);

    PROTECT(minv = coerceVector(minv, REALSXP));
    minvalue = REAL(minv);

    PROTECT(maxv = coerceVector(maxv, REALSXP));
    maxvalue = REAL(maxv);
    
    int ncell = INTEGER(ncl)[0];
    int ncode = INTEGER(ncd)[0];
    double cellarea = REAL(carea)[0];
    double minelasticity = REAL(minelas)[0];
    double maxelasticity = REAL(maxelas)[0];
    double maxiter = REAL(mi)[0];
    double maxdiff = REAL(md)[0];
    
    SEXP lu1; 			/* do this because we want to return lu1 */
    PROTECT(lu1 = allocVector(REALSXP, ncode * ncell));
    landuse1 = REAL(lu1);

    /* calculate potential */
    potential = (double *) malloc(ncode * ncell * sizeof(double));
    for (i = 0; i < ncode; i++) {
      for (j = 0; j < ncell; j++) {
	ix = i * ncell + j;
	potential[ix] = regr[ix] - landuse0[ix];
      }
    }

    /* calculate change direction */
    area0 = (double *) malloc(ncode * sizeof(double));
    area0 = calculateArea(area0, landuse0, cellarea, ncell, ncode);

    changedir = (double *) malloc(ncode * sizeof(double));

    for (i = 0; i < ncode; i++) {
      cd = demand[i] - area0[i];

      if (cd > 0) {
	changedir[i] = 1;

      } else if (cd < 0) {
	changedir[i] = -1;

      } else {
	changedir[i] = 0;
      }
    }
    /* ppotential = (double *) malloc(ncode * ncell * sizeof(double)); */

    /* jitter_f = 0.01; */
    count = 0;
    allocation_ok = 0;
    maxadjust = maxdiff;
    flagflex = 0;
   
    area1 = (double *) malloc(ncode * sizeof(double));

    GetRNGstate();
    do {

      /* jitter prob */
      /* for (i = 0; i < (ncode * ncell); i++) { */
      /*     if (! R_IsNA(potential[i])) { */
      /*         ppotential[i] = potential[i] + (-jitter_f + (jitter_f - (-jitter_f)) * unif_rand()); */
      /*     } else { */
      /*         ppotential[i] = potential[i]; */
      /*     } */
      /* } */
      
      landuse1 = computeChange(landuse1, landuse0, potential, elasticity, minchange, maxchange, changedir, changerule, minvalue, maxvalue, ncell, ncode);
      area1 = calculateArea(area1, landuse1, cellarea, ncell, ncode);
      /* for (i = 0; i < ncode; i++) { */
      /* 	Rprintf("%f ", area1[i]); */
      /* } */
      /* Rprintf("\n"); */

      landuse1 = correctCellChange(landuse1, landuse0, changedir, changerule, minvalue, maxvalue, ncell, ncode);
      area1 = calculateArea(area1, landuse1, cellarea, ncell, ncode);

      /* for debugging */
      /* for (i = 0; i < ncode; i++) { */
      /* 	Rprintf("%f ", elasticity[i]); */
      /* } */
      /* Rprintf("\n"); */
      /* for (i = 0; i < ncode; i++) { */
      /* 	Rprintf("%f ", area1[i]); */
      /* } */
      /* Rprintf("\n\n"); */
      
      tmpmaxdiff = 0;
      for (i = 0; i < ncode; i++) {
	ludirect = changedir[i];

	if (ludirect == 0) {

	  if (demand[i] >= area1[i]) {
	    ludirect = 1;
	  } else {
	    ludirect = -1;
	  }
	}

	if (ludirect == 1) {
	  elasticity[i] = elasticity[i] * (demand[i] / area1[i]);
	} else {
	  elasticity[i] = elasticity[i] * (area1[i] / demand[i]);
	}

	flag = 0;
	if (elasticity[i] > maxelasticity) {
	  flag = 1;
	  elasticity[i] = maxelasticity;
	}

	if (elasticity[i] < minelasticity) {
	  flag = 1;
	  if (changerule[i] < 0) {
	    elasticity[i] = minelasticity;
	  } else {
	    changedir[i] = changedir[i] * -1;
	  }
	}

	/* /\* update potential? *\/ */
	/* for (j = 0; j < ncell; j++) { */
	/*   ix = i * ncell + j; */
	/*   potential[ix] = regr[ix] - landuse1[ix]; */
	/* } */

	diff = fabs(area1[i] - demand[i]);

	if (diff > tmpmaxdiff) {
	  tmpmaxdiff = diff;
	}

	/* tot = tot + fabs(area[i] - demand[i]); */

      }

      if (tmpmaxdiff <= maxadjust) {
	allocation_ok = 1;
      } else {
	count = count + 1;
	/* if (count > maxiter * 0.5 & flagflex == 0) { */
	/*   maxadjust = maxadjust * 2; /\* is this an original feature? *\/ */
	/*   flagflex = 1; */
	/* } */
      }
      
    } while (count < maxiter && allocation_ok == 0);
    PutRNGstate();

    if (count >= maxiter && allocation_ok == 0) {
      Rprintf("Demand for current time point not met:\n");
      Rprintf("Demand:\n");
      for (i = 0; i < ncode; i++) {
	Rprintf("%f ", demand[i]);
      }
      Rprintf("\nAllocated:\n");
      for (i = 0; i < ncode; i++) {
      	Rprintf("%f ", area1[i]);
      }
      Rprintf("\n");
    }
    
    free(potential);
    free(area0);
    free(changedir);
    free(area1);

    UNPROTECT(10);
    return(lu1);

}
