#' Class ExpVarRasterStack
#'
#' An S4 class for explanatory variables.
#'
#' @slot filename see \code{raster::\link[raster]{Raster-class}}
#' @slot layers see \code{raster::\link[raster]{Raster-class}}
#' @slot title see \code{raster::\link[raster]{Raster-class}}
#' @slot extent see \code{raster::\link[raster]{Raster-class}}
#' @slot rotated see \code{raster::\link[raster]{Raster-class}}
#' @slot rotation see \code{raster::\link[raster]{Raster-class}}
#' @slot ncols see \code{raster::\link[raster]{Raster-class}}
#' @slot nrows see \code{raster::\link[raster]{Raster-class}}
#' @slot crs see \code{raster::\link[raster]{Raster-class}}
#' @slot history see \code{raster::\link[raster]{Raster-class}}
#' @slot z see \code{raster::\link[raster]{Raster-class}}
#' @slot index data.frame TODO
#'
#' @export
#' @exportClass ExpVarRasterStack
#' @rdname ExpVarRasterStack-class
setClass("ExpVarRasterStack",
         contains = c("RasterStack"),
         slots = c(index = "data.frame"),
         validity = function(object) {
             check1 <- raster::nlayers(object) == nrow(object@index)
             if (!check1) stop()
             check2 <- ncol(object@index) == 3
             if (!check2) stop()
             return(TRUE)           
         }
)
