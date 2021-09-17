#' @include length.R names.R ExpVarRasterStack.R PerformanceList.R
NULL

#' Subset objects
#'
#' Extract a subset of objects from container classes such as
#' \code{ExpVarRasterStack}, \code{PredictiveModelList}, \code{PredictionList} and
#' \code{PerformanceList}.
#'
#' @param x an object of class \code{ExpVarRasterStack},
#'   \code{PredictiveModelList}, \code{PredictionList} or \code{PerformanceList}
#' @param subset integer or character indicating the objects to be extracted
#' @param ... additional arguments (none)
#' @param drop logical
#'
#' @return Subsetted object, possibly simplified
#' 
#' @export
#' @rdname subset-methods
#'
#' @examples
#'
#' ## Sibuyan Island
#'
#' lu <- DiscreteLulcRasterStack(x=stack(sibuyan$maps[1:2]),
#'                               categories=c(1,2,3,4,5),
#'                               labels=c("forest","coconut","grass","rice","other"),
#'                               t=c(0,14))
#'
#' 
#' summary(lu)
#' lu <- subset(lu, subset=names(lu)[1])
#' summary(lu)
#' 
#' ## load explanatory variables
#' idx <- data.frame(var=paste("ef_", formatC(1:13, width=3, flag=0)),
#'                   yr=rep(0,13),
#'                   dynamic=rep(FALSE,13))
#'
#' ef <- ExpVarRasterStack(x=stack(sibuyan$maps[3:15]), index=idx)
#' 
#' summary(ef)
#' ef <- subset(ef, subset=1:5)
#' summary(ef)
#'

## # rdname subset-methods
## # aliases subset,ExpVarRasterStack-method
## setMethod("subset", signature(x="ExpVarRasterStack"), 
##           function(x, subset, ...) {
##               subset <- .getsubset(x, subset)
##               x[[subset]]
##           }
##           )

#' @rdname subset-methods
#' @aliases subset,PredictiveModelList-method
setMethod("subset", signature(x="PredictiveModelList"), 
          function(x, subset, drop=FALSE, ...) {
              subset <- .getsubset(x, subset)
              if (length(subset) == 1) {
                  if (drop) {
                      x <- x@models[[subset]]
                  } else {                      
                      x <- new("PredictiveModelList",  ## is this the behaviour we want?
                               models=list(x@models[[subset]]),
                               categories=x@categories[subset],
                               labels=x@labels[subset])
                  }
                      
              } else {
                  x <- new("PredictiveModelList",
                           models=x@models[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,PerformanceList-method
setMethod("subset", signature(x="PerformanceList"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid performance object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid performance object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("PerformanceList",
                           performance=list(x@performance[[subset]]),
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("PerformanceList",
                           performance=x@performance[subset],
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,PredictionList-method
setMethod("subset", signature(x="PredictionList"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid prediction object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid prediction object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("PredictionList",
                           prediction=list(x@prediction[[subset]]),
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("PredictionList",
                           prediction=x@prediction[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )


.getsubset <- function(x, subset) {
    if (is.character(subset)) {
        i <- na.omit(match(subset, names(x)))
        if (length(i)==0) {
            stop('invalid object names')
        } else if (length(i) < length(subset)) {
            warning('invalid object names omitted')
        }
        subset <- i
    }
    subset <- as.integer(subset)
    if (! all(subset %in% 1:length(x))) {
        stop('not a valid subset')
    }
    subset
}
