#' @include class-Model.R
#' @include class-NeighbRasterStack.R
#' @include class-LulcRasterStack.R
#' @include class-ExpVarRasterStack.R
#' @include class-PredictiveModelList.R
NULL

#' Create a ClueModel object
#'
#' Methods to create a \code{ClueModel} object to supply to
#' \code{\link{allocate}}.
#'
#' @param observed.lulc an LulcRasterStack
#' @param explanatory.variables an ExpVarRasterStack object
#' @param predictive.models a PredictiveModelList object
#' @param time numeric vector containing timesteps over which simulation will
#'   occur  
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial observed land use map (i.e. the land use map for
#'   time 0)
#' @param elasticity Initial elasticity value. Default is 0.1
#' @param change.rule numeric vector specifying for each land use whether change
#'   is allowed in either direction (0), allowed in the direction of demand only
#'   (-1) or not allowed (1)
#' @param min.elasticity Minimum elasticity value. Default is 0.001
#' @param max.elasticity Maximum elasticity value. Default is 1.5
#' @param min.value numeric vector indicating the minimum fraction of each land
#'   use in a given cell
#' @param max.value numeric vector indicating the maximum fraction of each land
#'   use in a given cell
#' @param min.change numeric vector indicating for each land use the minimum
#'   amount of change that is allowed to occur in one time step
#' @param max.change numeric vector indicating for each land use the maximum
#'   amount of change that is allowed to occur in one time step
#' @param max.iteration The maximum number of iterations allowed at each time
#'   step
#' @param max.difference The maximum allowable difference between demand and
#'   allocated area
#' @param cell.area The area of each grid cell in the study region, which should
#'   have the same units as the demand
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{ClueModel-class}}, \code{\link{allocate}}
#'
#' @return A ClueModel object.
#'
#' @export
#' @rdname ClueModel
#'
#' @references
#' Veldkamp, A., & Fresco, L. O. (1996). CLUE-CR: an integrated multi-scale model
#' to simulate land use change scenarios in Costa Rica. Ecological modelling,
#' 91(1), 231-248.
#'
#' Verburg, P.H., & Bouma, J. (1999). Land use change under conditions of high
#' population pressure: the case of Java. Global environmental change, 9(4),
#' 303-312.
#'
#' @examples
#'
#' ## see lulcc2-package examples

ClueModel <- function(observed.lulc,        #observedLU
                      explanatory.variables,  #explanatoryVariables
                      predictive.models,    #predictiveModels
                      ## subset,               #subset
                      time,                 #time
                      demand,               #demand
                      elasticity=0.1,       #elasticity
                      change.rule,          #changeRule
                      min.elasticity=0.001, #minElasticity
                      max.elasticity=1.5,   #maxElasticity
                      min.value,            #minValue
                      max.value,            #maxValue
                      min.change,           #minChange
                      max.change,           #maxChange
                      max.iteration=1000,   #maxIteration
                      max.difference,       #maxDifference
                      cell.area) {     

    ## if (missing(subset))
    ##   subset <- complete.cases(raster::getValues(as(observed.lulc[[1]], "RasterStack")))

    ## default parameter values
    ## n <- length(observed.lulc@categories)
    ## if (missing(change.rule)) change.rule <- rep(-1, n)
    ## if (missing(min.value))   min.value <- rep(0, n)
    ## if (missing(max.value))   max.value <- rep(1, n)
    ## if (missing(min.change))  min.change <- rep(0, n)
    ## if (missing(max.change))  max.change <- rep(1, n)
    
    out <- new("ClueModel", observed.lulc=observed.lulc, explanatory.variables=explanatory.variables, predictive.models=predictive.models, time=time, demand=demand, elasticity=elasticity, change.rule=change.rule, min.elasticity=min.elasticity, max.elasticity=max.elasticity, min.value=min.value, max.value=max.value, min.change=min.change, max.change=max.change, max.iteration=max.iteration, max.difference=max.difference, cell.area=cell.area, categories=observed.lulc@categories, labels=observed.lulc@labels)

}

#' Create a CluesModel object
#'
#' Methods to create a \code{CluesModel} object to supply to
#' \code{\link{allocate}}.
#'
#' @param observed.lulc an LulcRasterStack
#' @param explanatory.variables an ExpVarRasterStack object
#' @param predictive.models a PredictiveModelList object
#' @param time numeric vector containing timesteps over which simulation will
#'   occur  
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial observed land use map (i.e. the land use map for
#'   time 0)
#' @param history RasterLayer containing land use history (values represent the
#'   number of years the cell has contained the current land use category)
#' @param mask RasterLayer containing binary values where 0 indicates cells
#'   that are not allowed to change
#' @param neighbourhood an object of class NeighbRasterStack
#' @param transition.rules matrix with land use change decision rules
#' @param neighbourhood.rules numeric with neighbourhood decision rules
#' @param elasticity numeric indicating the elasticity of each land use category to
#'   change. Elasticity varies between 0 and 1, with 0 indicating a low
#'   resistance to change and 1 indicating a high resistance to change
#' @param iteration.factor TODO,
#' @param max.iteration The maximum number of iterations allowed at each time
#'   step
#' @param max.difference The maximum allowable difference between demand and
#'   allocated area
#' @param ave.difference The maximum allowable average difference across all land
#'   uses
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{CluesModel-class}}, \code{\link{allocate}}
#'
#' @return A CluesModel object.
#'
#' @export
#' @rdname CluesModel
#'
#' @references
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
#' S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
#' model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' ## see lulcc2-package examples

CluesModel <- function(observed.lulc, 
                       explanatory.variables,
                       predictive.models,
                       time,
                       demand,
                       history=NULL,
                       mask=NULL,
                       neighbourhood=NULL,
                       transition.rules,
                       neighbourhood.rules=NULL,
                       elasticity,
                       iteration.factor=0.00001,
                       max.iteration=1000,
                       max.difference=5,
                       ave.difference=5,
                       ...) {

    ## if (missing(history)) {
    ##     history <- observed.lulc[[1]]
    ##     history[!is.na(history)] <- 1
    ## } 

    ## if (missing(mask)) {
    ##     mask <- observed.lulc[[1]]
    ##     mask[!is.na(mask)] <- 1
    ## }

    out <- new("CluesModel", observed.lulc=observed.lulc, explanatory.variables=explanatory.variables, predictive.models=predictive.models, time=time, demand=demand, history=history, mask=mask, neighbourhood=neighbourhood, transition.rules=transition.rules, neighbourhood.rules=neighbourhood.rules, elasticity=elasticity, iteration.factor=iteration.factor, max.iteration=max.iteration, max.difference=max.difference, ave.difference=ave.difference, categories=observed.lulc@categories, labels=observed.lulc@labels)

}
           
## setGeneric("CluesModel", function(obs, ef, models, ...)
##            standardGeneric("CluesModel"))

## # rdname CluesModel
## # aliases CluesModel,LulcRasterStack,ExpVarRasterStack,PredictiveModelList-method
## setMethod("CluesModel", signature(obs = "LulcRasterStack", ef = "ExpVarRasterStack", models = "PredictiveModelList"),
##           function(obs, ef, models, time, demand, hist, mask, neighb=NULL, elas, rules=NULL, nb.rules=NULL, params, output=NULL, ...) {

##               ## check that all maps have the same projection
##               cr <- lapply(ef@maps, FUN=function(x) compareRaster(obs, x, extent=FALSE, rowcol=FALSE, crs=TRUE, res=FALSE, orig=FALSE, stopiffalse=TRUE))

##               ## check x and models refer to the same categories
##               if (!all(obs@categories == models@categories)) {
##                   stop("'models' does not correspond with land use categories in 'obs'")
##               }

##               ## check dimensions of demand and time
##               if (ncol(demand) != length(obs@categories)) {
##                   stop("number of columns in 'demand' must equal number of land use categories")
##               }              
##               if (nrow(demand) != length(time)) {
##                   stop("number of rows in 'demand' must equal number of timesteps in 't'")
##               }

##               ## check whether hist and mask exist and have correct extent
##               if (missing(hist)) {
##                   hist <- obs[[1]]
##                   hist[!is.na(hist)] <- 1
##               } 

##               if (missing(mask)) {
##                   mask <- obs[[1]]
##                   mask[!is.na(mask)] <- 1
##               } 

##               ## create neighbourhood maps if required and check dimensions of nb.rules
##               if (!is.null(neighb)) {
##                   if (!is(neighb, "NeighbRasterStack")) stop("'neighb' should be an object of class 'NeighbRasterStack'")
##                   ## recalculate neighbourhood for initial observed map
##                   neighb <- NeighbRasterStack(x=obs[[1]], neighb=neighb)                  
##               }
              
##               if (!is.null(rules)) {
##                   if (!all(dim(rules) %in% length(obs@categories))) {
##                       stop("'rules' must be square matrix with dimensions equal to number of land use categories")
##                   }
##               } 

##               if (!is.null(neighb) && !is.null(nb.rules)) {
##                   if (length(nb.rules) != length(neighb)) {
##                       stop("rule should be provided for each neighbourhood map")
##                   }
                  
##               } else if (is.null(neighb) && !is.null(nb.rules)) {
##                   warning("neighb is NULL: neighbourhood decision rules not implemented")
##                   nb.rules <- NULL
##               }

##               if (length(elas) != length(obs@categories)) {
##                   stop("'elas' must be numeric vector with length equal to number of land use categories")
##               }

##               if (missing(params)) {
##                   params <- .checkCluesParams()
##               } else {
##                   params <- .checkCluesParams(params)
##               }
                  
##               out <- new("CluesModel", obs=obs, ef=ef, models=models, time=time, demand=demand, hist=hist, mask=mask, neighb=neighb, elas=elas, rules=rules, nb.rules=nb.rules, params=params, categories=obs@categories, labels=obs@labels, output=output)
             
##           }
## )

## .checkCluesParams <- function(params) {
##     if (missing(params) || length(params) == 0) {
##         params <- list(jitter.f=0.0001, scale.f=0.0005, max.iter=1000, max.diff=5, ave.diff=5)
##     } else {
##         if (is.null(names(params)) || any(nchar(names(params)) == 0)) stop("'params' must be a named list") 
##         if (!"jitter.f" %in% names(params)) params <- c(params, list(jitter.f=0.0001))
##         if (!"scale.f" %in% names(params))  params <- c(params, list(scale.f=0.0005))
##         if (!"max.iter" %in% names(params)) params <- c(params, list(max.iter=1000))
##         if (!"max.diff" %in% names(params)) params <- c(params, list(max.diff=5))
##         if (!"ave.diff" %in% names(params)) params <- c(params, list(ave.diff=5))
##         ## TODO check reasonable values
##     }
##     params <- params[c("jitter.f","scale.f","max.iter","max.diff","ave.diff")]
## }

## # Create an OrderedModel object
## #
## # Methods to create a \code{OrderedModel} object to supply to
## # \code{\link{allocate}}.
## #
## # The \code{params} argument is a list of parameter values which should contain
## # the following components:
## # 
## # \describe{
## #   \item{\code{max.diff}}{The maximum allowed difference between allocated and
## #     demanded area of any land use type. Default is 5}
## # }
## # 
## # param obs an LulcRasterStack object
## # param ef an ExpVarRasterStack object
## # param models a PredictiveModelList object
## # param time numeric vector containing timesteps over which simulation will
## #   occur  
## # param demand matrix with demand for each land use category in terms of number
## #   of cells to be allocated. The first row should be the number of cells
## #   allocated to the initial observed land use map (i.e. the land use map for
## #   time 0)
## # param hist RasterLayer containing land use history (values represent the
## #   number of years the cell has contained the current land use category)
## # param mask RasterLayer containing binary values where 0 indicates cells
## #   that are not allowed to change
## # param neighb an object of class NeighbRasterStack
## # param rules matrix with land use change decision rules
## # param nb.rules numeric with neighbourhood decision rules
## # param order numeric vector of land use categories in the order that change
## #   should be allocated. See Details
## # param params list with model parameters
## # param output either a RasterStack containing output maps or NULL
## # param \dots additional arguments (none)
## #
## # seealso \code{\link{OrderedModel-class}}, \code{\link{allocate}}
## #
## # return An OrderedModel object.
## #
## # export
## # rdname OrderedModel
## #
## # references
## # Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
## # high-resolution and harmonized model approach for reconstructing and analysing
## # historic land changes in Europe, Biogeosciences, 10:1543-1559.
## #
## # examples
## #
## # ## see lulcc2-package examples

#' Create a OrderedModel object
#'
#' Methods to create a \code{OrderedModel} object to supply to
#' \code{\link{allocate}}.
#'
#' @param observed.lulc an LulcRasterStack
#' @param explanatory.variables an ExpVarRasterStack object
#' @param predictive.models a PredictiveModelList object
#' @param time numeric vector containing timesteps over which simulation will
#'   occur  
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial observed land use map (i.e. the land use map for
#'   time 0)
#' @param history RasterLayer containing land use history (values represent the
#'   number of years the cell has contained the current land use category)
#' @param mask RasterLayer containing binary values where 0 indicates cells
#'   that are not allowed to change
#' @param neighbourhood an object of class NeighbRasterStack
#' @param transition.rules matrix with land use change decision rules
#' @param neighbourhood.rules numeric with neighbourhood decision rules
#' @param order numeric vector of land use categories in the order that change
#'   should be allocated
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{OrderedModel-class}}, \code{\link{allocate}}
#'
#' @return A OrderedModel object.
#'
#' @export
#' @rdname OrderedModel
#'
#' @references
#' Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
#' high-resolution and harmonized model approach for reconstructing and analysing
#' historic land changes in Europe, Biogeosciences, 10:1543-1559.
#'
#' @examples
#'
#' ## see lulcc2-package examples

OrderedModel <- function(observed.lulc, 
                         explanatory.variables,
                         predictive.models,
                         time,
                         demand,
                         history=NULL,
                         mask=NULL,
                         neighbourhood=NULL,
                         transition.rules,
                         neighbourhood.rules=NULL,
                         order, ...) {

    ## if (missing(history)) {
    ##     ## history <- observed.lulc[[1]]
    ##     ## history[!is.na(history)] <- 1
    ## } 

    ## if (missing(mask)) {
    ##     ## mask <- observed.lulc[[1]]
    ##     ## mask[!is.na(mask)] <- 1
    ## }

    out <- new("OrderedModel", observed.lulc=observed.lulc, explanatory.variables=explanatory.variables, predictive.models=predictive.models, time=time, demand=demand, history=history, mask=mask, neighbourhood=neighbourhood, transition.rules=transition.rules, neighbourhood.rules=neighbourhood.rules, order=order, categories=observed.lulc@categories, labels=observed.lulc@labels)

}

## setGeneric("OrderedModel", function(obs, ef, models, ...)
##            standardGeneric("OrderedModel"))

## # rdname OrderedModel
## # aliases OrderedModel,LulcRasterStack,ExpVarRasterStack,PredictiveModelList-method
## setMethod("OrderedModel", signature(obs = "LulcRasterStack", ef = "ExpVarRasterStack", models = "PredictiveModelList"),
##           function(obs, ef, models, time, demand, hist, mask, neighb=NULL, rules=NULL, nb.rules=NULL, order, params, output=NULL, ...) {

##               ## check x and models refer to the same categories
##               if (!all(obs@categories == models@categories)) {
##                   stop("'models' does not correspond with land use categories in 'obs'")
##               }

##               ## check dimensions of demand and time
##               if (ncol(demand) != length(obs@categories)) {
##                   stop("number of columns in 'demand' must equal number of land use categories")
##               }              
##               if (nrow(demand) != length(time)) {
##                   stop("number of rows in 'demand' must equal number of timesteps in 't'")
##               }

##               ## check whether hist and mask exist and have correct extent
##               if (missing(hist)) {
##                   hist <- obs[[1]]
##                   hist[!is.na(hist)] <- 1
##               } 

##               if (missing(mask)) {
##                   mask <- obs[[1]]
##                   mask[!is.na(mask)] <- 1
##               } 

##               ## create neighbourhood maps if required and check dimensions of nb.rules
##               if (!is.null(neighb)) {
##                   if (!is(neighb, "NeighbRasterStack")) stop("'neighb' should be an object of class 'NeighbRasterStack'")
##                   ## recalculate neighbourhood for initial observed map
##                   neighb <- NeighbRasterStack(x=obs[[1]], neighb=neighb)                  
##               }

##               if (!is.null(rules)) {
##                   if (!all(dim(rules) %in% length(obs@categories))) {
##                       stop("'rules' must be square matrix with dimensions equal to number of land use categories")
##                   }
##               } 

##               if (!is.null(neighb) && !is.null(nb.rules)) {
##                   if (length(nb.rules) != length(neighb)) {
##                       stop("rule should be provided for each neighbourhood map")
##                   }
                  
##               } else if (is.null(neighb) && !is.null(nb.rules)) {
##                   warning("neighb is NULL: neighbourhood decision rules not implemented")
##                   nb.rules <- NULL
##               }

##               if (missing(order)) {
##                   stop("missing argument 'order'")
##               } else {
##                   if (!all(order %in% obs@categories)) {
##                       stop("argument 'order' should contain exactly the same categories as categories (but not necessarily in the same order)")
##                   }
##               }
              
##               if (missing(params)) {
##                   params <- .checkOrderedParams()
##               } else {
##                   params <- .checkOrderedParams(params)
##               }
              
##               out <- new("OrderedModel", obs=obs, ef=ef, models=models, time=time, demand=demand, hist=hist, mask=mask, neighb=neighb, rules=rules, nb.rules=nb.rules, order=order, params=params, categories=obs@categories, labels=obs@labels, output=output)
             
##           }
## )

## .checkOrderedParams <- function(params) {
##     params <- list()
## }

##     if (missing(params) || length(params) == 0) {
##         params <- list(max.diff=5)
##     } else {
##         if (is.null(names(params)) || any(nchar(names(params)) == 0)) stop("'params' must be a named list") 
##         if (!all(c("max.diff") %in% params)) {
##             stop("'params' does not contain the required parameter set")
##         }
##         ## TODO check reasonable values
##     }
##     params <- params[c("max.diff")]
## }
