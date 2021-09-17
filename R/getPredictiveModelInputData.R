#' @include class-LulcRasterStack.R class-ExpVarRasterStack.R as.data.frame.R
NULL

#' Extract data to fit predictive models
#'
#' Extract a data.frame containing variables required for fitting predictive
#' models. Column names correspond to the names of lu and ef.
#'
#' @param lu an LulcRasterStack object
#' @param ef an ExpVarRasterStack object
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param ... additional arguments to \link{as.data.frame}
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{LulcRasterStack}},
#' \code{\link{ExpVarRasterStack}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname getPredictiveModelInputData
#' 
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#'
#' lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
#'                               categories=c(1,2,3),
#'                               labels=c("Forest","Built","Other"),
#'                               t=c(0,6,14))
#'
#' idx <- data.frame(var=paste("ef_", formatC(1:3, width=3, flag=0)),
#'                   yr=rep(0,3),
#'                   dynamic=rep(FALSE,3))
#' 
#' ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=idx)
#'
#' part <- partition(x=lu, size=0.1, spatial=TRUE, t=0)
#'
#' train.data <- getPredictiveModelInputData(lu=lu,
#'                                           ef=ef,
#'                                           cells=part[["train"]],
#'                                           t=0)
#' dim(train.data)
#' names(train.data)
#'
#' }
#'

getPredictiveModelInputData <- function(lu, ef, cells, ...) {
    ludf <- as.data.frame(lu, cells=cells, ...)
    efdf  <- as.data.frame(ef, cells=cells, ...)
    cbind(ludf, efdf)
}
