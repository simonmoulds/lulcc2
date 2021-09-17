#' @include class-ExpVarRasterStack.R
NULL

#' Create an ExpVarRasterStack object
#'
#' Methods to create an ExpVarRasterStack object, which may be created from file
#' or an existing Raster* object.
#'
#' Inductive and deductive land use change models predict the allocation of
#' change based on spatially explicit biophysical and socioeconomic covariates.
#' These may be static, such as elevation or geology, or dynamic, such as maps of
#' population density or road networks. To identify whether a covariable is
#' static or dynamic a data frame is supplied to the ExpVarRasterStack
#' constructor function with three columns: the first column specifies the name
#' of the variable, the second column specifies the time point for which it is
#' relevant and the third column specifies whether it is dynamic or not. Data frame
#' rows should correspond to the individual layers of the RasterStack object
#' containing the explanatory variables. If dynamic variables are used it is not
#' necessary to supply a map for each time point in the simulation: during
#' allocation the most recent map will automatically be selected.
#' 
#' @param x Raster* object
#' @param index data.frame
#' @param \dots additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @seealso \code{raster::\link[raster]{stack}}
#' @return An ExpVarRasterStack object.
#'
#' @export
#' @rdname ExpVarRasterStack-methods
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#' 
#' idx <- data.frame(var=paste("ef_", formatC(1:3, width=3, flag=0)),
#'                   yr=rep(0,3),
#'                   dynamic=rep(FALSE,3))
#' 
#' ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=idx)
#' 
#' ## Sibuyan
#' 
#' idx <- data.frame(var=paste("ef_", formatC(1:13, width=3, flag=0)),
#'                   yr=rep(0,13),
#'                   dynamic=rep(FALSE,13))
#'
#' ef <- ExpVarRasterStack(x=stack(sibuyan$maps[3:15]), index=idx)
#'

setGeneric("ExpVarRasterStack", function(x, ...)
           standardGeneric("ExpVarRasterStack"))

#' @rdname ExpVarRasterStack-methods
#' @aliases ExpVarRasterStack,character-method
setMethod("ExpVarRasterStack", signature(x = "character"),
          function(x, ...) {
              ExpVarRasterStack(x=stack(x), ...)
          }
          )

#' @rdname ExpVarRasterStack-methods
#' @aliases ExpVarRasterStack,RasterStack-method
setMethod("ExpVarRasterStack", signature(x = "RasterStack"),
          function(x, index, ...) {
              new("ExpVarRasterStack", x, index=index)
          }
          )

