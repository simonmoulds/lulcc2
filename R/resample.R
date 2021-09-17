#' @include class-ExpVarRasterStack.R
NULL

#' Resample maps in ExpVarRasterStack object or list 
#'
#' A wrapper function for \code{raster::\link[raster]{resample}} to resample
#' raster objects in an ExpVarRasterStack object or list.
#'
#' @param x an ExpVarRasterStack object or list of Raster* maps to be resampled
#' @param y Raster* object with parameters that \code{x} should be resampled to
#' @param method method used to compute values for the new RasterLayer, should be
#'   \code{"bilinear"} for bilinear interpolation, or \code{"ngb"} for nearest
#'   neighbour
#' @param \dots additional arguments to \code{raster::\link[raster]{resample}}
#'
#' @seealso \code{\link{ExpVarRasterStack}}, \code{raster::\link[raster]{resample}}
#'
#' @return An ExpVarRasterStack object or list, depending on \code{x}.
#'
#' @export
#' @rdname resample
#'
#' @examples
#'
#' ## see lulcc2-examples

#' @rdname resample
#' @aliases resample,ExpVarRasterStack,Raster-method
setMethod("resample", signature(x = "ExpVarRasterStack", y = "Raster"),
          function(x, y, method="ngb", ...) {
              resamp.maps <- vector(mode="list", length=nlayers(x))
              ## if (nlayers(x) > 0) {
              for (i in 1:nlayers(x)) {
                  resamp.maps[[i]] <- raster::resample(x=x[[i]], y=y, method=method, ...)
              }
              ## } else {
              ##     resamp.maps <- maps
              ## }
              ExpVarRasterStack(stack(resamp.maps), index=x@index)
          }
)

#' @rdname resample
#' @aliases resample,list,Raster-method
setMethod("resample", signature(x = "list", y = "Raster"),
          function(x, y, method="ngb", ...) {
              for (i in 1:length(x)) {
                  x[[i]] <- raster::resample(x=x[[i]], y=y, method=method, ...)
              }
              x
          }
)
