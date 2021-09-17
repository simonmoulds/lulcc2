#' @include class-LulcRasterStack.R LulcRasterStack.R
NULL

setMethod("crop", c("ContinuousLulcRasterStack","ANY"),
          function(x, y, ...) {
              xx <- crop(as(x, "RasterStack"), y, ...)
              stack(xx)
              ## ContinuousLulcRasterStack(stack(xx),
              ##                              categories=x@categories,
              ##                              labels=x@labels,
              ##                              t=x@t)
          }
          )
              
              
              
