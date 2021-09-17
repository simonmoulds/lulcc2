#' Predict allocation suitability
#'
#' Estimate allocation suitability with predictive models.
#'
#' This function is usually called from \code{allocate} to calculate land use
#' suitability at each timestep. However, it may also be used to produce
#' suitability maps (see examples).
#'
#' @param object a PredictiveModelList object 
#' @param newdata data.frame containing new data
#' @param data.frame logical indicating whether the function should return a
#'   matrix (default) or data.frame
#' @param \dots additional arguments to \code{predict} methods
#'
#' @seealso \code{\link[stats]{predict}}, \code{\link{allocate}}
#'
#' @return A matrix.
#'
#' @export
#' @rdname predict
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
#' idx <- data.frame(var=c("ef_001","ef_002","ef_003"),
#'                   yr=c(0,0,0),
#'                   dynamic=c(FALSE,FALSE,FALSE))
#' ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=idx)
#'
#' part <- partition(x=lu, size=0.1, spatial=TRUE, t=0)
#' train.data <- getPredictiveModelInputData(lu=lu,
#'                                           ef=ef,
#'                                           cells=part[["train"]],
#'                                           t=0)
#'
#' forest.form <- as.formula("Forest ~ ef_001 + ef_002")
#' built.form <- as.formula("Built ~ ef_001 + ef_002 + ef_003")
#' other.form <- as.formula("Other ~ ef_001 + ef_002")
#'
#' forest.glm <- glm(forest.form, family=binomial, data=train.data)
#' built.glm <- glm(built.form, family=binomial, data=train.data)
#' other.glm <- glm(other.form, family=binomial, data=train.data)
#' glm.mods <- PredictiveModelList(list(forest.glm, built.glm, other.glm),
#'                                 categories=lu@@categories,
#'                                 labels=lu@@labels)
#'
#' all.data <- as.data.frame(x=ef, cells=part[["all"]]) 
#' probmaps <- predict(object=glm.mods, 
#'                     newdata=all.data, 
#'                     data.frame=TRUE) 
#'
#' points <- rasterToPoints(lu[[1]], spatial=TRUE) 
#' probmaps <- SpatialPointsDataFrame(points, probmaps) 
#' probmaps <- rasterize(x=probmaps, y=lu[[1]], 
#'                       field=names(probmaps))
#' plot(probmaps)
#'
#' }
#' 

#' @rdname predict
#' @method predict PredictiveModelList
#' @export
predict.PredictiveModelList <- function(object, newdata, data.frame=FALSE, ...) {
    out <- list()
    for (i in 1:length(object)) {

        mod <- object@models[[i]]
        if (inherits(mod, "randomForest")) {
            if (mod$type %in% "classification") {
                pred <- predict(object=mod, newdata=newdata, type="prob", ...)
                ix <- which(colnames(pred) %in% "1")
                out[[i]] <- pred[,ix]
            } else {
                out[[i]] <- predict(object=mod, newdata=newdata)##, ...)
            }
        }

        if (inherits(mod, "rpart")) {
            out[[i]] <- predict(object=mod, newdata=newdata, ...)
        }

        if (inherits(mod, "lm")) {
            out[[i]] <- predict(object=mod, newdata=newdata, ...)
        }
            
        if (inherits(mod, "glm")) {
            out[[i]] <- predict(object=mod, newdata=newdata, type="response", ...)
        }
    }

    names(out) <- object@labels
    out <- as.data.frame(out)
    if (!data.frame) out <- as.matrix(out)
    out
}

## # rdname predict
## # aliases predict,PredictiveModelList-method
## setMethod("predict","PredictiveModelList",predict.PredictiveModelList)

## # rdname predict
## # method predict PredictiveModelList
## # export
## predict.PredictiveModelList <- function(object, newdata, data.frame=FALSE, time, ...) {
##     prob <- vector(mode="list", length=(length(object@categories) * length(time)))

##     ## TODO: avoid repeating raster::predict if no dynamic variables are used (copy first map)

##     for (i in 1:length(time)) {
##         d <- as(.getExpVarRasterStack(x=newdata, time=time[i]), "RasterStack")                       
##         for (j in 1:length(object)) {
##             ix <- (i-1) * length(object) + j
##             prob[[ix]] <- raster::predict(object=d, model=object[[j]], ...)
##         }
##     }
##     stack(prob)
## }

## # rdname predict
## # aliases predict,PredictiveModelList-method
## setMethod("predict", "PredictiveModelList", predict.PredictiveModelList)
