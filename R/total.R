#' Total number of cells in a Raster* object
#' 
#' Count the area or number of cells belonging to each category in a Raster*
#' object.
#'
#' If x is a DiscreteLulcRasterStack object this function returns the number of
#' cells belonging to each category. If x is a ContinuousLulcRasterStack object
#' the function returns the sum of the fractions of the various land use
#' categories.
#'
#' @param x Raster* object
#' @param categories numeric vector containing land use categories. Only cells
#'   belonging to these categories will be counted
#' @param \dots additional arguments (none)
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{total}}{a matrix containing the total number of cells belonging
#'     to each category. Rows represent layers in the input Raster* object}
#'   \item{\code{categories}}{the categories included in the calculation}
#' }
#'
#' @useDynLib lulcc2
#'
#' @export
#' @rdname total-methods
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
#' total(x=lu)
#' total(x=lu[[1]])
#' total(x=lu[[2]])
#'

setGeneric("total", function(x, ...)
           standardGeneric("total"))

#' @rdname total-methods
#' @aliases total,DiscreteLulcRasterStack-method
setMethod("total", "DiscreteLulcRasterStack",
          function(x, categories, ...) {

              if (missing(categories)) {
                  categories <- x@categories
              }

              area <- matrix(data=NA, nrow=raster::nlayers(x), ncol=length(categories))
              for (i in 1:raster::nlayers(x)) {
                  vals <- raster::getValues(x[[i]])
                  area[i,] <- .Call("total", vals, categories, PACKAGE='lulcc2')
              }

              list(total=area, categories=categories)
          }
          )

#' @rdname total-methods
#' @aliases total,ContinuousLulcRasterStack-method
setMethod("total", "ContinuousLulcRasterStack",
          function(x, categories, ...) {
              
              if (missing(categories)) {
                  categories <- x@categories
              }

              area <- matrix(data=NA, nrow=length(x@t), ncol=length(categories))
              for (i in 1:length(x@t)) {
                  st <- as(x[[i]], "RasterStack")
                  for (j in 1:length(categories)) {
                      ix <- which(x@categories %in% categories[j])
                      area[i,j] <- sum(getValues(st[[ix]]), na.rm=TRUE) ##* prod(res(x))
                  }
              }

              list(total=area, categories=categories)
          }
          )

#' @rdname total-methods
#' @aliases total,Raster-method
setMethod("total", "Raster",
          function(x, categories, ...) {
    
              if (missing(categories)) {
                  warning("missing argument 'categories': getting categories from 'x'")
                  categories <- sort(unique(as.numeric(raster::getValues(x))))
              }
    
              area <- matrix(data=NA, nrow=raster::nlayers(x), ncol=length(categories))
              for (i in 1:raster::nlayers(x)) {
                  vals <- raster::getValues(x[[i]])
                  area[i,] <- .Call("total", vals, categories, PACKAGE='lulcc2')
              }
    
              list(total=area, categories=categories)
          }
          )
