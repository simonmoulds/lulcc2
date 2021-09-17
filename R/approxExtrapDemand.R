#' Extrapolate land use area in time
#'
#' Extrapolate land use area from two or more observed land use maps to provide
#' a valid (although not necessarily realistic) demand scenario.
#'
#' Many allocation routines, including the two included with \code{lulcc2},
#' require non-spatial estimates of land use demand for every timestep in the
#' study period. Some routines are coupled to complex economic models that
#' predict future or past land use demand based on economic considerations;
#' however, linear extrapolation of trends remains a useful technique.
#'
#' @param lu an LulcRasterStack object containing at least two maps
#' @param tout numeric vector specifying the timesteps where interpolation is to
#'   take place. Comparable to the \code{xout} argument of
#'   \code{Hmisc::\link[Hmisc]{approxExtrap}}
#' @param \dots additional arguments to \code{Hmisc::\link[Hmisc]{approxExtrap}}
#'
#' @return A matrix.
#'
#' @seealso \code{Hmisc::\link[Hmisc]{approxExtrap}}
#'
#' @export
#' @rdname approxExtrapDemand-methods
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## load observed land use maps
#' lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
#'                               categories=c(1,2,3),
#'                               labels=c("Forest","Built","Other"),
#'                               t=c(0,6,14))
#' 
#' ## obtain demand scenario by interpolating between observed maps
#' dmd <- approxExtrapDemand(lu=lu, tout=0:14)
#' 
#' ## plot
#' matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",
#'         lty=1, col=c("Green","Red","Blue"))
#' legend("topleft", legend=lu@@labels, col=c("Green","Red","Blue"), lty=1)
#' 
#' ## linear extrapolation is also possible
#' dmd <- approxExtrapDemand(lu=lu, tout=c(0:50))
#' 
#' ## plot
#' matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",
#'         lty=1, col=c("Green","Red","Blue"))
#' legend("topleft", legend=lu@@labels, col=c("Green","Red","Blue"), lty=1)
#'

setGeneric("approxExtrapDemand", function(lu, ...)
           standardGeneric("approxExtrapDemand"))

#' @rdname approxExtrapDemand-methods
#' @aliases approxExtrapDemand,LulcRasterStack-method
setMethod("approxExtrapDemand", "LulcRasterStack",
          function(lu, tout, ...) {              
              if (nlayers(lu) > 1) {
                  tot <- total(x=lu)$total
                  demand <- matrix(data=NA, nrow=length(tout), ncol=length(lu@categories))
                  for (i in 1:length(lu@categories)) {
                      x <- Hmisc::approxExtrap(lu@t, tot[,i], tout)$y
                      x[x < 0] <- 0
                      demand[,i] <- x
                  }

              } else {
                  stop("cannot estimate land use demand with only one map")
              }
              demand
              
          }
          )   

#' @rdname approxExtrapDemand-methods
#' @aliases approxExtrapDemand,DiscreteLulcRasterStack-method
setMethod("approxExtrapDemand", "DiscreteLulcRasterStack",
          function(lu, tout, ...) {              
              demand <- callNextMethod()
              ncell <- length(which(!is.na(raster::getValues(lu[[1]]))))
              demand <- roundSum(demand, n=ncell, digits=0)
              demand
          }
          )   

## # rdname approxExtrapDemand-methods
## # aliases approxExtrapDemand,ContinuousLulcRasterStack-method
## setMethod("approxExtrapDemand", "ContinuousLulcRasterStack",
##           function(obs, tout, totalArea, ...) {              
##               demand <- callNextMethod()
##               ## demand <- roundSum(demand, n=totalArea, digits=3)
##               demand
##           }
##           )   

#' Round elements in matrix or data.frame rows
#'
#' Round all numbers in a matrix or data.frame while ensuring that all rows sum
#' to the same value.
#'
#' The main application of \code{roundSum} is to ensure that each row in the
#' demand matrix specifies exactly the number of cells to be allocated to each
#' land use category for the respective timestep. It may also be used to convert
#' the units of demand to number of cells.
#'
#' @param x matrix or data.frame
#' @param n numeric specifying the target sum for each row in \code{x}
#' @param digits integer indicating the number of decimal places to be used
#' @param \dots additional arguments (none)
#'
#' @return A matrix.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Sibuyan Island
#'
#' ## load observed land use data and create demand scenario
#' obs <- LulcRasterStack(x=sibuyan$maps,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("Forest","Coconut","Grass","Rice","Other"),
#'                     t=c(0,14))
#' 
#' dmd <- approxExtrapDemand(obs, tout=0:14)
#' apply(dmd, 1, sum)
#' 
#' ## artificially perturb for illustration purposes
#' dmd <- dmd * runif(1)
#' apply(dmd, 1, sum)
#' 
#' ## use roundSum to correct demand scenario
#' ncell <- length(which(!is.na(getValues(sibuyan$maps$lu_sib_1997))))
#' ncell
#' dmd <- roundSum(dmd, ncell=ncell)
#' apply(dmd, 1, sum)
#'
#' }

roundSum <- function(x, n, digits=0, ...) {

    x <- x * 10 ^ digits
    for (i in 1:nrow(x)) {
        y <- as.numeric(x[i,])
        y[y < 0] <- 0                         ## negative numbers not allowed
        y <- y / sum(y) * (n * 10 ^ digits)   ## scale row to ensure it sums to n
        xint <- floor(y)                      ## convert x to integer
        diff <- y - floor(y)                  ## roundoff error TODO: tolerance?
        diff <- sort(diff, index.return=TRUE) ## sort diff by roundoff error
        tot.diff <- (n * 10 ^ digits) - sum(floor(y))
        if (tot.diff > 0) {
            ix <- seq((length(y) -tot.diff + 1), length(y), 1)
            ix <- diff$ix[ix]
            xint[ix] <- xint[ix] + 1
        }
        x[i,] <- xint
    }
    x <- x / 10 ^ digits
    x
}

## roundSum <- function(x, ncell, ...) {
    
##     for (i in 1:nrow(x)) {
##         y <- as.numeric(x[i,])
##         y[y < 0] <- 0                         ## negative numbers not allowed
##         y <- y / sum(y) * ncell               ## scale row to ensure it sums to ncell
##         xint <- floor(y)                      ## convert x to integer
##         diff <- y - floor(y)                  ## roundoff error TODO: tolerance?
##         diff <- sort(diff, index.return=TRUE) ## sort diff by roundoff error
##         tot.diff <- ncell - sum(floor(y))
##         if (tot.diff > 0) {
##             ix <- seq((length(y)-tot.diff+1), length(y), 1)
##             ix <- diff$ix[ix]
##             xint[ix] <- xint[ix] + 1
##         }
##         x[i,] <- xint
##     }
##     x
## }


