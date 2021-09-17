#' Partition raster data 
#'
#' Divide a categorical raster map into training and testing partitions.
#' A wrapper function for \cr
#' \code{caret::\link[caret]{createDataPartition}} (Kuhn, 2008).
#' 
#' @param x RasterLayer with categorical data
#' @param size numeric value between zero and one indicating the proportion of
#'   non-NA cells that should be included in the training partition. Default is
#'   0.5, which results in equally sized partitions
#' @param spatial logical. If TRUE, the function returns a SpatialPoints object
#'   with the coordinates of cells in each partition. If FALSE, the cell numbers
#'   are returned
#' @param t numeric corresponding to one of the time points for which a land use
#'   map is available. 
#' @param \dots additional arguments (none)
#'
#' @seealso \code{caret::\link[caret]{createDataPartition}}
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{train}}{a SpatialPoints object or numeric vector indicating the
#'   cells in the training partition}
#'   \item{\code{test}}{a SpatialPoints object or numeric vector indicating the
#'   cells in the testing partition}
#'   \item{\code{all}}{a SpatialPoints object or numeric vector indicating all
#'   non-NA cells in the study region}
#' }
#'
#' @export
#' @rdname partition-methods
#'
#' @references Kuhn, M. (2008). Building predictive models in R using the caret
#' package. Journal of Statistical Software, 28(5), 1-26.
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
#' part <- partition(x=lu, size=0.05, spatial=TRUE, t=0)
#'
#' plot(lu[[1]])
#' points(part[["train"]])
#'
#' }

setGeneric("partition", function(x, ...)
           standardGeneric("partition"))

#' @rdname partition-methods
#' @aliases partition,RasterLayer-method
setMethod("partition", "RasterLayer",
          function(x, size=0.5, spatial=TRUE, ...) {
              
              points <- raster::rasterToPoints(x, spatial=TRUE)
              cells <- raster::cellFromXY(x, points)
              train.ix <- caret::createDataPartition(y=points@data[,1], p=size, list=FALSE, times=1)[,1]
              if (spatial) {
                  points <- as(points, "SpatialPoints")
                  train <- points[train.ix]
                  test <- points[-train.ix]
                  all <- points
              } else {
                  train <- cells[train.ix]
                  test <- cells[-train.ix]
                  all <- cells
              }
              list(train=train, test=test, all=all)
          }
          )

#' @rdname partition-methods
#' @aliases partition,DiscreteLulcRasterStack-method
setMethod("partition", "DiscreteLulcRasterStack",
          function(x, size=0.5, spatial=TRUE, t, ...) {
              
              ix <- which(x@t %in% t)
              partition(x[[ix]], size=size, spatial=spatial, ...)
          }
          )

#' @rdname partition-methods
#' @aliases partition,ContinuousLulcRasterStack-method
setMethod("partition", "ContinuousLulcRasterStack",
          function(x, size=0.5, spatial=TRUE, ...) {

              x <- as(x, "RasterStack")
              points <- raster::rasterToPoints(x[[1]], spatial=TRUE)
              cells <- raster::cellFromXY(x, points)
              n <- length(points)
              train.ix <- sample(seq_len(length(points)), round(n * size), replace=FALSE)
              
              if (spatial) {
                  points <- as(points, "SpatialPoints")
                  train <- points[train.ix]
                  test <- points[-train.ix]
                  all <- points
              } else {
                  train <- cells[train.ix]
                  test <- cells[-train.ix]
                  all <- cells
              }
              list(train=train, test=test, all=all)
          }
          )

              
## partition <- function(x, size=0.5, spatial=TRUE, ...) {
##     points <- raster::rasterToPoints(x, spatial=TRUE)
##     cells <- raster::cellFromXY(x, points)
##     train.ix <- caret::createDataPartition(y=points@data[,1], p=size, list=FALSE, times=1)[,1]
##     if (spatial) {
##         points <- as(points, "SpatialPoints")
##         ## if (size == 1) {
##         ##     train <- points
##         ##     test <- points
##         ## } else {
##         train <- points[train.ix]
##         test <- points[-train.ix]
##         ## }
##         all <- points
##     } else {
##         ## if (size == 1) {
##         ##     train <- cells
##         ##     test <- cells
##         ## } else {
##         train <- cells[train.ix]
##         test <- cells[-train.ix]
##         ## }
##         all <- cells
##     }
##     out <- list(train=train, test=test, all=all)
## }
