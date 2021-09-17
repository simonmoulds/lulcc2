#' @include class-PredictiveModelList.R class-LulcRasterStack.R class-ExpVarRasterStack.R as.data.frame.R
NULL

#' Create PredictiveModelList object 
#'
#' Crete an object of class PredictiveModelList.
#'
#' @param models list containing predictive models
#' @param categories numeric vector of land use categories in observed maps
#' @param labels character vector with labels corresponding to \code{categories}
#' @param ... additional arguments (none)
#'
#' @return A PredictiveModelList object
#'
#' @export
#' @rdname PredictiveModelList-methods
#'
#' @examples
#'
#' ## see lulcc2-package examples

PredictiveModelList <- function(models, categories, labels, ...) {
    new("PredictiveModelList", models=models, categories=categories, labels=labels)
}

## # Fit predictive models 
## #
## # These functions fit parametric and non-parametric models to data.
## #
## # param formula list containing formula objects
## # param family see \code{\link[stats]{glm}}. Default is 'binomial'. Only used by
## #   \code{glmModels}
## # param obs TODO
## # param categories TODO
## # param labels TODO
## # param ... additional arguments to specific functions
## #
## # seealso \code{\link[stats]{glm}},
## #   \code{randomForest::\link[randomForest]{randomForest}}
## #
## # return A PredictiveModelList object
## #
## # name Model fitting
## # rdname Model-fitting
## #
## # examples
## #
## # ## see lulcc2-package examples
## NULL

## # export
## # rdname Model-fitting
## lmModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     lm.models <- vector(mode="list", length=length(formula))
##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)

##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         lm.models[[i]] <- lm(form, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=lm.models,
##                categories=categories,
##                labels=labels)
## }
    

## # export 
## # rdname Model-fitting
## glmModels <- function(formula, family=binomial, ..., obs, categories=NA, labels=NA) {
    
##     glm.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         glm.models[[i]] <- glm(form, family=family, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=glm.models,
##                categories=categories,
##                labels=labels)
## }

## # export
## # rdname Model-fitting
## randomForestModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rf.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rf.models[[i]] <- randomForest::randomForest(form, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rf.models,
##                categories=categories,
##                labels=labels)
## }

## ## # export
## ## # rdname Model-fitting
## ## rpartModels <- function(formula, ..., obs, categories=NA, labels=NA) {

## ##     rpart.models <- list()

## ##     if (!missing(obs)) {
## ##         categories <- obs@categories
## ##         labels <- obs@labels
## ##     }
## ##     formula <- .checkFormula(formula, categories, labels)
    
## ##     for (i in 1:length(formula)) {
## ##         form <- formula[[i]]
## ##         rpart.models[[i]] <- rpart::rpart(form, method="class", ...)
## ##     }

## ##     out <- new("PredictiveModelList",
## ##                models=rpart.models,
## ##                categories=categories,
## ##                labels=labels)
## ## }

.checkFormula <- function(formula, categories, labels) {
    
    dep <- sapply(formula, function(x) as.character(x)[2])
    if (length(categories) != length(labels))
      stop("'labels' must correspond to 'categories'")

    if (!all(labels %in% dep))
      stop("a formula must be supplied for each land use type")

    formula <- formula[match(dep, labels)]
}

## .getPredictiveModelInputData <- function(lu, ef, cells, ...) {
##     ludf <- as.data.frame(lu, cells=cells, ...)
##     efdf  <- as.data.frame(ef, cells=cells, ...)
##     df    <- cbind(ludf, efdf)
##     df
## }

## # rdname Model-fitting
## # exportMethod lmModels
## setGeneric("lmModels", function(lu, ef, ...)
##            standardGeneric("lmModels"))

## # rdname Model-fitting
## # aliases lmModels,ContinuousLulcRasterStack,list-method
## setMethod("lmModels", c("ContinuousLulcRasterStack","ExpVarRasterStack"),
##           function(lu, ef, time, subset, formula, ...) {

##               lm.models <- vector(mode="list", length=length(formula))
##               formula <- .checkFormula(formula, lu@categories, lu@labels)

##               data <- .getPredictiveModelInputData(lu=lu, ef=ef, cells=subset, t=time)
              
##               for (i in 1:length(formula)) {
##                   form <- formula[[i]]
##                   lm.models[[i]] <- lm(form, data=data, ...)
##               }

##               out <- new("ContinuousPredictiveModelList",
##                          models=lm.models,
##                          categories=lu@categories,
##                          labels=lu@labels)
##           }
##           )

## # rdname Model-fitting
## # exportMethod glmModels
## setGeneric("glmModels", function(lu, ef, ...)
##            standardGeneric("glmModels"))

## # rdname Model-fitting
## # aliases glmModels,DiscreteLulcRasterStack,ExpVarRasterStack-method
## setMethod("glmModels", c("DiscreteLulcRasterStack","ExpVarRasterStack"),
##           function(lu, ef, time, subset, formula, family=binomial, model=FALSE, ...) {

##               glm.models <- vector(mode="list", length=length(formula))
##               formula <- .checkFormula(formula, lu@categories, lu@labels)

##               data <- .getPredictiveModelInputData(lu=lu, ef=ef, cells=subset, t=time)

##               for (i in 1:length(formula)) {
##                   form <- formula[[i]]
##                   glm.models[[i]] <- glm(form, family=family, model=model, ...)
##               }

##               out <- new("DiscretePredictiveModelList",
##                          models=glm.models,
##                          categories=lu@categories,
##                          labels=lu@labels)
##           }
##           )

## # rdname Model-fitting
## # exportMethod randomForestModels
## setGeneric("randomForestModels", function(lu, ef, ...)
##            standardGeneric("randomForestModels"))

## # rdname Model-fitting
## # aliases randomForestModels,DiscreteLulcRasterStack,ExpVarRasterStack-method
## setMethod("randomForestModels", c("DiscreteLulcRasterStack","ExpVarRasterStack"),
##           function(lu, ef, time, subset, formula, ...) {

##               rf.models <- vector(mode="list", length=length(formula))
##               formula <- .checkFormula(formula, lu@categories, lu@labels)
              
##               data <- .getPredictiveModelInputData(lu=lu, ef=ef, cells=subset, t=time)

##               for (i in 1:length(formula)) {
##                   form <- formula[[i]]
##                   rf.models[[i]] <- randomForest::randomForest(form, data=data, ...)
##               }

##               out <- new("DiscretePredictiveModelList",
##                          models=rf.models,
##                          categories=lu@categories,
##                          labels=lu@labels)
##           }
##           )

## # rdname Model-fitting
## # aliases randomForestModels,ContinuousLulcRasterStack,ExpVarRasterStack-method
## setMethod("randomForestModels", c("ContinuousLulcRasterStack","ExpVarRasterStack"),
##           function(lu, ef, time, subset, formula, ...) {
##               rf.models <- vector(mode="list", length=length(formula))
##               formula <- .checkFormula(formula, lu@categories, lu@labels)
              
##               data <- .getPredictiveModelInputData(lu=lu, ef=ef, cells=subset, t=time)

##               for (i in 1:length(formula)) {
##                   form <- formula[[i]]
##                   rf.models[[i]] <- randomForest::randomForest(form, data=data, ...)
##               }

##               out <- new("ContinuousPredictiveModelList",
##                          models=rf.models,
##                          categories=lu@categories,
##                          labels=lu@labels)
##           }
##           )
              

## # export
## # rdname Model-fitting
## rpartModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rpart.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rpart.models[[i]] <- rpart::rpart(form, method="class", ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rpart.models,
##                categories=categories,
##                labels=labels)
## }
