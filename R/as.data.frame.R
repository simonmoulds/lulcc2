#' @include class-ExpVarRasterStack.R class-Model.R
NULL

#' Coerce objects to data.frame
#'
#' This function extracts data from all raster objects in
#' \code{\link{LulcRasterStack}} or \code{\link{ExpVarRasterStack}} objects
#' for a specified timestep.
#'
#' If x is a DiscreteLulcRasterStack object the raster corresponding to t is
#' first transformed to a RasterBrick with a boolean layer for each class with
#' \code{raster::\link[raster]{layerize}}.
#'
#' @param x an ExpVarRasterStack or LulcRasterStack object
#' @param row.names NULL or a character vector giving the row.names for the
#'   data.frame. Missing values are not allowed
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param t numeric indicating the time under consideration
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{LulcRasterStack}},
#' \code{\link{ExpVarRasterStack}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname as.data.frame
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' 
#' ## load observed land use maps
#' lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
#'                               categories=c(1,2,3),
#'                               labels=c("Forest","Built","Other"),
#'                               t=c(0,6,14))
#' 
#' ## explanatory variables
#' idx <- data.frame(var=c("ef_001","ef_002","ef_003"),
#'                   yr=c(0,0,0),
#'                   dynamic=c(FALSE,FALSE,FALSE))
#' 
#' ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=idx)
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=lu[[1]], size=0.1, spatial=TRUE)
#' df1 <- as.data.frame(x=lu, cells=part[["all"]], t=0)
#' df2 <- as.data.frame(x=ef, cells=part[["all"]], t=0)
#' df <- cbind(df1,df2)
#'
#' }
#'

#' @rdname as.data.frame
#' @method as.data.frame ExpVarRasterStack
#' @export
as.data.frame.ExpVarRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {
    ix <- .ExpVarIndex(x, time=t)
    x <- x[[ix]]
    names(x) <- names(ix)
    ## x <- .getExpVarRasterStack(x, time=t)
    ## st <- as(x, "RasterStack")
    as.data.frame(extract(x, cells, ...))
}

#' @rdname as.data.frame
#' @method as.data.frame DiscreteLulcRasterStack
#' @export
as.data.frame.DiscreteLulcRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {

    if (!t %in% x@t) stop()
    ix <- which(x@t %in% t)
    br <- raster::layerize(x[[ix]])
    names(br) <- x@labels
    as.data.frame(raster::extract(x=br, y=cells))
}
    
#' @rdname as.data.frame
#' @method as.data.frame ContinuousLulcRasterStack
#' @export
as.data.frame.ContinuousLulcRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {

    if (!t %in% x@t) stop()
    ix <- which(x@t %in% t)
    br <- as(x[[ix]], "RasterStack")
    names(br) <- x@labels
    as.data.frame(raster::extract(x=br, y=cells, ...))
}

#' @rdname as.data.frame
#' @aliases as.data.frame,ExpVarRasterStack-method
setMethod("as.data.frame","ExpVarRasterStack",as.data.frame.ExpVarRasterStack)

#' @rdname as.data.frame
#' @aliases as.data.frame,DiscreteLulcRasterStack-method
setMethod("as.data.frame","DiscreteLulcRasterStack",as.data.frame.DiscreteLulcRasterStack)

#' @rdname as.data.frame
#' @aliases as.data.frame,ContinuousLulcRasterStack-method
setMethod("as.data.frame","ContinuousLulcRasterStack",as.data.frame.ContinuousLulcRasterStack)

#' Update data frame
#'
#' Function to update a data frame holding model variables with values of dynamic
#' covariables for a new time point. This function is used internally by
#' allocation routines.
#' 
#' @param x ExpVarRasterStack object
#' @param y data.frame to update
#' @param cells index of cells to be extracted and added to the data frame (see
#'   \code{\link{as.data.frame}})
#' @param time numeric indicating the time for which the data frame should be
#'   updated
#' @param \dots additional arguments (none)
#'
#' @return data.frame
#' @rdname updateDataFrame-methods
#' @export
#'
setGeneric("updateDataFrame", function(x, ...)
           standardGeneric("updateDataFrame"))

#' @rdname updateDataFrame-methods
#' @aliases updateDataFrame,ExpVarRasterStack-method
setMethod("updateDataFrame", "ExpVarRasterStack",
          function(x, y, cells, time, ...) {

              if (length(cells) != nrow(y))
                stop()

              ix1 <- .dynamicExpVarIndex(x, time=time)
              ix2 <- which(names(y) %in% names(ix1))
              y[,ix2] <- as.data.frame(extract(x[[ix1]], cells, ...))
              y
          }
          )

.staticExpVarIndex <- function(x) {
    index <- x@index
    static.ix <- which(!index[,3])
    setNames(static.ix, index[static.ix,1])
}

.dynamicExpVarIndex <- function(x, time) {
    index <- x@index
    dyn.ix <- index[,3]
    if (any(dyn.ix)) {
        dyn.vars <- unique(index[dyn.ix,1])
        return(setNames(sapply(seq_len(length(dyn.vars)), FUN=function(i) {
            var <- dyn.vars[i]
            t <- sort(index[(index[,1] %in% var), 2])
            t <- t[findInterval(time, t, all.inside=TRUE)]
            which(index[,1] %in% var & index[,2] %in% t)}), dyn.vars))
    } else {
        return(NULL)
    }
}
    
.ExpVarIndex <- function(x, time) {
    static.ix <- .staticExpVarIndex(x)
    dynamic.ix <- .dynamicExpVarIndex(x, time)
    sort(c(static.ix, dynamic.ix))
}

## .update.data.frame <- function(x, y, map, cells, t, ...) {
##     ## hidden function to update a data.frame containing dynamic explanatory variables
##     ##
##     ## Args:
##     ##   x: a data.frame
##     ##   y: an ExpVarRasterStack object
##     ##   map: ???
##     ##   cells: ???
##     ##   t: the time for which dynamic explanatory variables should be updated
##     ##
##     ## Returns:
##     ##   a data.frame
    
##     ix <- t + 1
##     nms <- names(x)
##     if (length(y@maps) > 0) {
##         dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1))))
##         if (length(dynamic.ix) > 0) {
##             s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
##             update.vals <- s[cells]
##             x[,dynamic.ix] <- update.vals
##         }
##     }

##     names(x) <- nms
##     x
## }

## .getExpVarRasterStack <- function(x, time) {
##     index <- x@index
##     static.ix <- !index[,3]
##     dyn.vars <- unique(index[!static.ix,1])
##     dyn.ix <- sapply(seq_len(length(dyn.vars)), FUN=function(i) {
##         var <- dyn.vars[i]
##         t <- sort(index[(index[,1] %in% var), 2])
##         t <- t[findInterval(time, t, all.inside=TRUE)]
##         which(index[,1] %in% var & index[,2] %in% t)})

##     ix <- sort(c(which(static.ix), dyn.ix))
##     ## setNames(x[[ix]], index[ix,1])
## }
