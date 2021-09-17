#' @include class-Model.R
NULL

#' Allocate land use change spatially
#'
#' Perform spatial allocation of land use change using different models.
#' Currently the function provides an implementation of the Change in Land Use
#' and its Effects (CLUE; Veldkamp and Fresco, 1996, Verburg et al., 1996),
#' CLUE at Small regional extent (CLUE-S; Verburg et al., 2002) and an ordered
#' procedure based on the algorithm described by Fuchs et al., (2013), modified
#' to allow stochastic transitions.
#'
#' @param model an object inheriting from class \code{Model}
#' @param stochastic logical
#' @param \dots additional arguments for specific methods
#'
#' @seealso \code{\link{CluesModel}}
#' @return LulcRasterStack.
#' @export
#' @rdname allocate
#'
#' @references
#' Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
#' high-resolution and harmonized model approach for reconstructing and analysing
#' historic land changes in Europe, Biogeosciences, 10:1543-1559.
#'
#' Veldkamp, A., & Fresco, L. O. (1996). CLUE-CR: an integrated multi-scale model
#' to simulate land use change scenarios in Costa Rica. Ecological modelling,
#' 91(1), 231-248.
#'
#' Verburg, P.H., & Bouma, J. (1999). Land use change under conditions of high
#' population pressure: the case of Java. Global environmental change, 9(4),
#' 303-312.
#'
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
#' S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
#' model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' ## see lulcc2-package examples

setGeneric("allocate", function(model, ...)
           standardGeneric("allocate"))

#' @rdname allocate
#' @aliases allocate,CluesModel-method
setMethod("allocate", signature(model = "CluesModel"),
          function(model, ...) {

              t0 <- model@time[1]
              lu0 <- model@observed.lulc[[which(model@observed.lulc@t %in% t0)]]
              lu0 <- as(lu0, "RasterLayer")
              cells <- which(complete.cases(raster::getValues(lu0)))
              lu0.vals <- extract(lu0, cells)
              newdata <- as.data.frame(x=model@explanatory.variables, cells=cells, t=t0)
              prob <- predict(object=model@predictive.models, newdata=newdata)

              if (!is.null(model@history)) {
                  hist.vals <- raster::extract(model@history, cells)
              } else {
                  hist.vals <- NULL
              }
              
              if (!is.null(model@mask)) {
                  mask.vals <- raster::extract(model@mask, cells)
              } else {
                  mask.vals <- NULL
              }

              ncell <- length(cells)
              ncode <- length(model@categories)
              nt <- length(model@time)
              any.dynamic <- any(model@explanatory.variables@index[,3])

              maps <- vector(mode="list", length=nt)
              maps[[1]] <- lu0

              for (i in 2:nt) {

                  t1 <- model@time[i]
                  ## d <- model@demand[i,]

                  if (any.dynamic && i > 2) {
                      newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=t1)
                      prob <- predict(object=model@predictive.models, newdata=newdata)
                  }
                  tprob <- prob

                  lu1.vals <- clues(lu0=lu0,
                                    lu0.vals=lu0.vals,
                                    tprob=tprob,
                                    nb=model@neighbourhood,
                                    nb.rules=model@neighbourhood.rules,
                                    transition.rules=model@transition.rules,
                                    hist.vals=hist.vals,
                                    mask.vals=mask.vals,
                                    demand=model@demand[c(i-1,i),,drop=FALSE],
                                    categories=model@categories,
                                    elasticity=model@elasticity,
                                    iteration.factor=model@iteration.factor,
                                    max.iteration=model@max.iteration,
                                    max.difference=model@max.difference,
                                    ave.difference=model@ave.difference)

                  lu1 <- raster::raster(lu0, ...) 
                  lu1[cells] <- lu1.vals
                  maps[[i]] <- lu1

                  ## for (j in 1:ncode) {
                  ##     ix <- lu0.vals %in% model@categories[j]
                  ##     tprob[ix,j] <- tprob[ix,j] + model@elasticity[j]
                  ## }

                  ## tprob <- .applyNeighbDecisionRules(model=model, x=lu0, tprob=tprob)
                  ## change.direction <- d - model@demand[(i-1),]
                  ## tprob <- .applyDecisionRules(model=model, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)
                  ## auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
                  ## lu0.vals[auto$ix] <- auto$vals
                  ## tprob[auto$ix,] <- NA

                  ## lu1.vals <- .clues(tprob=tprob,
                  ##                    lu0.vals=lu0.vals,
                  ##                    demand=d,
                  ##                    categories=model@categories,
                  ##                    iteration.factor=model@iteration.factor,
                  ##                    max.iteration=model@max.iteration,
                  ##                    max.difference=model@max.difference,
                  ##                    average.difference=model@ave.difference)

                  if (i < nt) {

                      if (!is.null(hist.vals)) {
                          hist.vals <- .updatehist(lu0.vals, lu1.vals, hist.vals)
                      }
                      
                      lu0 <- lu1
                      lu0.vals <- lu1.vals 
                  }
              }

              ## raster::stack(maps)
              DiscreteLulcRasterStack(x=raster::stack(maps),
                                      categories=model@categories,
                                      labels=model@labels,
                                      t=model@time)
              ## model@output <- raster::stack(maps)
              ## model     

          }
          )

#' CLUE-S
#'
#' Allocate land use change using the CLUE-S algorithm.
#'
#' @param lu0 RasterLayer showing initial land use
#' @param lu0.vals numeric containing non-NA values from \code{lu0}
#' @param tprob matrix with land use suitability values. Columns should
#'   correspond to \code{categories}, rows should correspond with \code{cells}
#' @param nb neighbourhood map. See CluesModel
#' @param nb.rules neighbourhood rules. See CluesModel documentation
#' @param transition.rules transition rules. See CluesModel documentation
#' @param hist.vals numeric vector detailing the number of consecutive time steps
#'   each cell has been allocated to its current land use
#' @param mask.vals numeric vector containing binary values where 0 indicates
#'   cells that are not allowed to change
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial land use map, the second row should be the number
#'   of cells to allocate in the subsequent time point
#' @param categories numeric vector containing land use categories
#' @param elasticity elasticity values. See CluesModel documentation
#' @param iteration.factor iteration factor. See CluesModel documentation
#' @param max.iteration The maximum number of iterations allowed at each time
#'   step
#' @param max.difference The maximum allowable difference between demand and
#'   allocated area
#' @param ave.difference The maximum allowable average difference across all land
#'   uses
#' @param \dots additional arguments (none)
#'
#' @return numeric vector with updated land use values.
#'
#' @useDynLib lulcc2
#'
#' @export
#' @rdname clues
#'
#' @examples
#'
#' ## See lulcc2-package examples

clues <- function(lu0, lu0.vals, tprob, nb=NULL, nb.rules=NULL, transition.rules=NULL, hist.vals=NULL, mask.vals=NULL, demand, categories, elasticity, iteration.factor, max.iteration, max.difference, ave.difference, ...) {

    ## add elasticity to tprob values    
    for (j in 1:length(categories)) {
        ix <- lu0.vals %in% categories[j]
        tprob[ix,j] <- tprob[ix,j] + elasticity[j]
    }

    ## apply neighbourhood rules
    tprob <- .applyNeighbDecisionRules(nb=nb, nb.rules=nb.rules, x=lu0, tprob=tprob, categories=categories)

    ## apply transition rules
    change.direction <- demand[2,] - demand[1,]
    tprob <- .applyDecisionRules(transition.rules=transition.rules, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)

    ## make automatic changes and set the probability of these cells to NA
    auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=categories, mask=mask.vals)
    lu0.vals[auto$ix] <- auto$vals
    tprob[auto$ix,] <- NA

    demand <- demand[2,,drop=TRUE]
    
    ## run CLUE-S algorithm
    lu1.vals <- .Call("allocateclues", tprob, lu0.vals, demand, categories, iteration.factor, max.iteration, max.difference, ave.difference)

    lu1.vals
    ## ## add values to RasterLayer
    ## lu1 <- raster::raster(lu0, ...) 
    ## lu1[cells] <- lu1.vals
    ## lu1
}

#' @rdname allocate
#' @aliases allocate,ClueModel-method
setMethod("allocate", signature(model = "ClueModel"),
          function(model, ...) {

              t0 <- model@time[1]
              lu0 <- model@observed.lulc[[(model@observed.lulc@t %in% t0)]]
              lu0 <- as(lu0, "RasterStack")
              cells <- which(complete.cases(raster::getValues(lu0)))
              cells <- xyFromCell(lu0, cells, spatial=TRUE) ######
              
              lu0.vals <- extract(lu0, cells)
              newdata <- as.data.frame(x=model@explanatory.variables, cells=cells, t=t0)
              regr <- predict(object=model@predictive.models, newdata=newdata)

              ncell <- length(cells)
              ncode <- length(model@categories)
              nt <- length(model@time)
              any.dynamic <- any(model@explanatory.variables@index[,3])
              
              maps <- vector(mode="list", length=nt)
              maps[[1]] <- lu0

              for (i in 2:nt) {

                  t1 <- model@time[i]; print(t1)
                  d <- model@demand[i,]

                  if (any.dynamic && (i > 1)) {
                      newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=model@time[i])
                      regr <- predict(object=model@predictive.models, newdata=newdata)
                  }

                  ## do something here to allow for fill?
                  if (!all(model@labels %in% colnames(regr))) {
                      regr1 <- matrix(data=0.5, nrow=nrow(regr), ncol=length(model@labels))
                      colnames(regr1) <- model@labels
                      ix <- match(colnames(regr), model@labels)
                      regr1[,ix] <- regr
                      regr <- regr1
                  }   

                  lu1.vals <- clue(lu0.vals=lu0.vals,
                                   regr=regr,
                                   demand=d,
                                   elasticity=model@elasticity,
                                   change.rule=model@change.rule,
                                   min.elasticity=model@min.elasticity,
                                   max.elasticity=model@max.elasticity,
                                   min.change=model@min.change,
                                   max.change=model@max.change,
                                   min.value=model@min.value,
                                   max.value=model@max.value,
                                   max.iteration=model@max.iteration,
                                   max.difference=model@max.difference,
                                   cell.area=model@cell.area,
                                   ncell=ncell,
                                   ncode=ncode)

                  lu1 <- lu0
                  lu1[cells] <- lu1.vals
                  maps[[i]] <- lu1

                  if (i < nt) {
                      lu0.vals <- matrix(data=lu1.vals, nrow=ncell)
                  }
              }

              ContinuousLulcRasterStack(x=raster::stack(maps),
                                        labels=model@labels,
                                        t=model@time)
              ## model@output <- ContinuousLulcRasterStack(x=raster::stack(maps),
              ##                                           labels=model@labels,
              ##                                           t=model@time)
              ## model
              
          }
)

#' CLUE
#'
#' Allocate land use change using the CLUE algorithm.
#'
#' @param lu0.vals matrix containing non-NA values from \code{lu0}
#' @param regr matrix containing...
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial land use map, the second row should be the number
#'   of cells to allocate in the subsequent time point
#' @param elasticity Initial elasticity value. Default is 0.1
#' @param change.rule numeric vector specifying for each land use whether change
#'   is allowed in either direction (0), allowed in the direction of demand only
#'   (-1) or not allowed (1)
#' @param min.elasticity Minimum elasticity value. Default is 0.001
#' @param max.elasticity Maximum elasticity value. Default is 1.5
#' @param min.change numeric vector indicating for each land use the minimum
#'   amount of change that is allowed to occur in one time step
#' @param max.change numeric vector indicating for each land use the maximum
#'   amount of change that is allowed to occur in one time step
#' @param min.value numeric vector indicating the minimum fraction of each land
#'   use in a given cell
#' @param max.value numeric vector indicating the maximum fraction of each land
#'   use in a given cell
#' @param max.iteration The maximum number of iterations allowed at each time
#'   step
#' @param max.difference The maximum allowable difference between demand and
#'   allocated area
#' @param cell.area The area of each grid cell in the study region, which should
#'   have the same units as the demand
#' @param ncell number of cells considered for change (equal to the length of
#'   \code{lu0.vals}
#' @param ncode number of land use categories under consideration
#' @param \dots additional arguments (none)
#'
#' @return numeric vector with updated land use values.
#'
#' @useDynLib lulcc2
#'
#' @export
#' @rdname clue
#'
#' @examples
#'
#' ## See lulcc2-package examples
#'

clue <- function(lu0.vals, regr, demand, elasticity, change.rule, min.elasticity, max.elasticity, min.change, max.change, min.value, max.value, max.iteration, max.difference, cell.area, ncell, ncode) {
    
    lu1.vals <- .Call("allocateclue", regr, lu0.vals, demand, elasticity, change.rule, cell.area, min.elasticity, max.elasticity, min.change, max.change, min.value, max.value, max.iteration, max.difference, ncell, ncode)
    lu1.vals

}

#' @rdname allocate
#' @aliases allocate,OrderedModel-method
setMethod("allocate", signature(model = "OrderedModel"),
          function(model, stochastic=TRUE, ...) {
              t0 <- model@time[1]
              lu0 <- model@observed.lulc[[which(model@observed.lulc@t %in% t0)]]
              lu0 <- as(lu0, "RasterLayer")
              cells <- which(complete.cases(raster::getValues(lu0)))
              lu0.vals <- extract(lu0, cells)
              newdata <- as.data.frame(x=model@explanatory.variables, cells=cells, t=t0)
              prob <- predict(object=model@predictive.models, newdata=newdata)

              if (!is.null(model@history)) {
                  hist.vals <- raster::extract(model@history, cells)
              } else {
                  hist.vals <- NULL
              }
              
              if (!is.null(model@mask)) {
                  mask.vals <- raster::extract(model@mask, cells)
              } else {
                  mask.vals <- NULL
              }

              ncell <- length(cells)
              ncode <- length(model@categories)
              nt <- length(model@time)
              any.dynamic <- any(model@explanatory.variables@index[,3])

              maps <- vector(mode="list", length=nt)
              maps[[1]] <- lu0
              ## map0 <- model@obs[[1]]
              ## cells <- which(!is.na(raster::getValues(map0)))
              ## map0.vals <- raster::extract(map0, cells)
              ## if (!is.null(model@hist)) hist.vals <- raster::extract(model@hist, cells) else NULL
              ## if (!is.null(model@mask)) mask.vals <- raster::extract(model@mask, cells) else NULL
              ## newdata <- as.data.frame(x=model@ef, cells=cells)
              ## prob <- predict(object=model@models, newdata=newdata)
              ## maps <- raster::stack(map0)

              for (i in 2:nt) {

                  ## d <- model@demand[(i+1),]

                  ## ## 1. update land use suitability matrix if dynamic factors exist
                  ## if (model@ef@dynamic && i > 1) {
                  ##     newdata <- .update.data.frame(x=newdata, y=model@ef, map=map0, cells=cells, timestep=(i-1))
                  ##     prob <- predict(object=model@models, newdata=newdata)
                  ## }
                  ## tprob <- prob

                  ## ## 2. implement neighbourhood decision rules
                  ## tprob <- .applyNeighbDecisionRules(model=model, x=map0, tprob=tprob)

                  ## ## 3. implement other decision rules
                  ## cd <- d - model@demand[i,] ## change direction
                  ## tprob <- .applyDecisionRules(model=model, x=map0.vals, hist=hist.vals, cd=cd, tprob=tprob)

                  ## ## 4. make automatic conversions if necessary
                  ## auto <- .autoConvert(x=map0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
                  ## map0.vals[auto$ix] <- auto$vals
                  ## tprob[auto$ix,] <- NA
                  t1 <- model@time[i]
                  d <- model@demand[i,]

                  if (any.dynamic && i > 2) {
                      newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=t1)
                      prob <- predict(object=model@predictive.models, newdata=newdata)
                  }
                  tprob <- prob

                  lu1.vals <- ordered(lu0.vals, tprob, d, model@order, model@categories, stochastic, 100)
                  
                  ## lu1.vals <- ordered_old(lu0=lu0,
                  ##                         lu0.vals=lu0.vals,
                  ##                         tprob=tprob,
                  ##                         nb=model@neighbourhood,
                  ##                         nb.rules=model@neighbourhood.rules,
                  ##                         transition.rules=model@transition.rules,
                  ##                         hist.vals=hist.vals,
                  ##                         mask.vals=mask.vals,
                  ##                         demand=model@demand[c(i-1,i),,drop=FALSE],
                  ##                         categories=model@categories,
                  ##                         order=model@order,
                  ##                         stochastic=stochastic)

                  lu1 <- raster::raster(lu0, ...) 
                  lu1[cells] <- lu1.vals
                  maps[[i]] <- lu1

                  ## tprob <- .applyNeighbDecisionRules(model=model, x=lu0, tprob=tprob)
                  ## change.direction <- d - model@demand[(i-1),]
                  ## tprob <- .applyDecisionRules(model=model, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)
                  ## auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
                  ## lu0.vals[auto$ix] <- auto$vals
                  ## tprob[auto$ix,] <- NA

                  ## ## 5. allocation
                  ## lu1.vals <- .ordered(tprob=tprob, lu0.vals=lu0.vals, demand=d, categories=model@categories, order=model@order, stochastic=stochastic)

                  ## lu1 <- raster::raster(lu0, ...) 
                  ## lu1[cells] <- lu1.vals
                  ## maps[[i]] <- lu1

                  if (i < nt) {

                      if (!is.null(hist.vals)) {
                          hist.vals <- .updatehist(lu0.vals, lu1.vals, hist.vals)
                      }
                      
                      lu0 <- lu1
                      lu0.vals <- lu1.vals 
                  }
                  ## map1 <- raster::raster(map0, ...) 
                  ## map1[cells] <- map1.vals
                  ## maps <- raster::stack(maps, map1)

                  ## ## 6. prepare model for next timestep
                  ## if (i < nrow(model@demand)) {
                  ##     if (!is.null(model@hist)) hist.vals <- .updatehist(map0.vals, map1.vals, hist.vals) 
                  ##     map0 <- map1
                  ##     map0.vals <- map1.vals 
                  ## }
              }
               
              DiscreteLulcRasterStack(x=raster::stack(maps),
                                      categories=model@categories,
                                      labels=model@labels,
                                      t=model@time)
              ## model@output <- maps
              ## model     
          }
)

#' Ordered allocation
#'
#' Allocate land use change using the ordered algorithm.
#'
#' @param lu0 RasterLayer showing initial land use
#' @param lu0.vals numeric containing non-NA values from \code{lu0}
#' @param tprob matrix with land use suitability values. Columns should
#'   correspond to \code{categories}, rows should correspond with \code{cells}
#' @param nb neighbourhood map. See CluesModel
#' @param nb.rules neighbourhood rules. See CluesModel documentation
#' @param transition.rules transition rules. See CluesModel documentation
#' @param hist.vals numeric vector detailing the number of consecutive time steps
#'   each cell has been allocated to its current land use
#' @param mask.vals numeric vector containing binary values where 0 indicates
#'   cells that are not allowed to change
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial land use map, the second row should be the number
#'   of cells to allocate in the subsequent time point
#' @param categories numeric vector containing land use categories
#' @param order numeric vector of land use categories in the order that change
#'   should be allocated
#' @param stochastic Logical indicating whether or not the allocation routine
#'   should be run in stochastic mode
#' @param \dots additional arguments (none)
#'
#' @return numeric vector with updated land use values.
#'
#' @useDynLib lulcc2
#'
#' @export
#' @rdname ordered_old
#'
#' @examples
#'
#' ## See lulcc2-package examples
ordered_old <- function(lu0, lu0.vals, tprob, nb=NULL, nb.rules=NULL, transition.rules=NULL, hist.vals=NULL, mask.vals=NULL, demand, categories, order, stochastic) {

    ## apply neighbourhood rules
    tprob <- .applyNeighbDecisionRules(nb=nb, nb.rules=nb.rules, x=lu0, tprob=tprob, categories=categories)

    ## apply transition rules
    change.direction <- demand[2,] - demand[1,]
    tprob <- .applyDecisionRules(transition.rules=transition.rules, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)

    ## make automatic changes and set the probability of these cells to NA
    auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=categories, mask=mask.vals)
    lu0.vals[auto$ix] <- auto$vals
    tprob[auto$ix,] <- NA

    demand <- demand[2,,drop=TRUE]

    ## initial condition
    lu0.area <- .Call("total", lu0.vals, categories)
    diff <- demand - lu0.area
    if (sum(abs(diff)) == 0) return(lu0.vals)                
    lu1.vals <- lu0.vals
    
    for (i in 1:length(order)) {
        
        ix <- which(categories %in% order[i])
        cat <- categories[ix]
        n <- demand[ix] - length(which(lu1.vals %in% cat))   ## number of cells to convert

        ## static demand
        if (n == 0) {
            ixx <- which(lu0.vals %in% cat)                  ## index of all cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }
        
        ## increasing demand
        if (n > 0) {
            ixx <- which(!lu1.vals %in% cat)                 ## index of all cells not currently belonging to lu
            p <- tprob[ixx,ix]                                ## suitability of all cells not currently belonging to lu (NB will include NAs)
            p.ix <- order(p, na.last=TRUE, decreasing=TRUE)   ## index of cells when arranged from high to low
            p <- p[p.ix]                                      ## suitability arranged from high to low
            p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
            p <- p[which(!is.na(p))]                          ## suitability with NAs removed
            ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in lu1.vals)     
            #p.range <- range(p, na.rm=TRUE); print(p.range)                   
            #p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability (0-1)

            ## repeat {
            ##     select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
            ##     if (length(select.ix) >= abs(n)) break()      ## only exit loop if select.ix includes enough cells to meet demand
            ## }

            if (stochastic) {
                counter <- 0
                repeat {
                    counter <- counter + 1
                    select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
                    if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
                }

            } else {
                select.ix <- seq(1, length(p))
            }
            
            select.ix <- select.ix[1:n]                       ## select cells with the highest suitability
            ixx <- ixx[select.ix]                             ## index
            lu1.vals[ixx] <- cat                             ## allocate change
            ixx <- which(lu1.vals %in% cat)                  ## index of cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }

        ## decreasing demand
        if (n < 0) {
            ixx <- which(lu0.vals %in% cat)                  ## index of all cells currently belonging to lu
            p <- tprob[ixx,ix]                                ## suitability of all cells currently belonging to lu (will include NAs)
            p.ix <- order(p, na.last=TRUE, decreasing=FALSE)   ## index of cells when arranged low to high
            p <- p[p.ix]                                      ## suitability arranged from low to high
            p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
            p <- p[which(!is.na(p))]                          ## suitability with NAs removed
            ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in lu1.vals)  
            ## p.range <- range(p, na.rm=TRUE)                   
            ## p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability
            if (stochastic) {
                counter <- 0
                repeat {
                    counter <- counter + 1
                    select.ix <- which(p < runif(length(p)))      ## compare suitability to numbers drawn from random normal distribution 
                    if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
                }
            } else {
                select.ix <- seq(1, length(p))
            }

            select.ix <- select.ix[1:abs(n)]                       ## select cells with lowest suitability
            ixx <- ixx[select.ix]                             ## index 
            lu1.vals[ixx] <- -1                              ## unclassified
            ixx <- which(lu1.vals %in% cat)                  ## index of cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }
    }
    lu1.vals
}

################################################################################

## helper functions

.applyNeighbDecisionRules <- function(nb, nb.rules, x, tprob, categories) {
    if (!is.null(nb) && !is.null(nb.rules)) {
        nb.allow <- allowNeighb(neighb=nb, x=x, categories=categories, rules=nb.rules)
        tprob <- tprob * nb.allow
    } 
    tprob
}

.applyDecisionRules <- function(transition.rules, x, hist, cd, tprob, categories) {
    if (!is.null(transition.rules) && !is.null(hist)) {
        allow <- allow(x=x, hist=hist, categories=categories, cd=cd, rules=transition.rules)
        tprob <- tprob * allow
    }
    tprob
}

# useDynLib lulcc2
.updatehist <- function(lu0, lu1, hist) {
    hist <- updatehist(lu0, lu1, hist)
    ## hist <- .Call("updatehist", lu0, lu1, hist)
    hist
}

.maxtprob <- function(x) {    
    if (length(which(!is.na(x)) > 0)) {
        out <- max(x, na.rm=TRUE)
    } else {
        out <- NA
    }
}

# useDynLib lulcc2
.autoConvert <- function(x, prob, categories, mask=NULL, ...) {
    if (!is.null(mask) && length(x) != length(mask)) stop("mask must have same length as x")
    if (is.null(mask)) mask <- rep(1, length(x))
    ## TODO: change autoconvert function so mask is optional
    ## vals <- .Call("autoconvert", x, mask, prob, categories)
    vals <- autoconvert(x, mask, prob, categories)
    ix <- which(!is.na(vals))
    vals <- vals[ix]
    out <- list(ix=ix, vals=vals)
}

## .applyNeighbDecisionRules <- function(model, x, tprob) {
##     if (!is.null(model@neighbourhood) && !is.null(model@neighbourhood.rules)) {
##         nb.allow <- allowNeighb(neighb=model@neighbourhood, x=x, categories=model@categories, rules=model@neighbourhood.rules)
##         tprob <- tprob * nb.allow
##     } 
##     tprob
## }

## .applyDecisionRules <- function(model, x, hist, cd, tprob) {
##     if (!is.null(model@transition.rules)) {
##         allow <- allow(x=x, hist=hist, categories=model@categories, cd=cd, rules=model@transition.rules)
##         tprob <- tprob * allow
##     }
##     tprob
## }

## .allocate <- function(model, fun, ...) {              

##     map0 <- model@obs[[1]]
##     cells <- which(!is.na(raster::getValues(map0)))
##     map0.vals <- raster::extract(map0, cells)
##     hist.vals <- raster::extract(model@hist, cells)
##     mask.vals <- raster::extract(model@mask, cells)
##     newdata <- as.data.frame(x=model@pred, cells=cells)
##     prob <- predict(object=model@models, newdata=newdata)
##     maps <- raster::stack(map0)
              
##     for (i in 1:(nrow(model@demand) - 1)) {
##          print(i)                                    
##          d <- model@demand[(i+1),] ## demand for current timestep
##          if (model@pred@dynamic && i > 1) {
##              newdata <- .update.data.frame(x=newdata, y=model@pred, map=map0, cells=cells, timestep=(i-1))
##              prob <- predict(object=model@models, newdata=newdata)
##          }
##          tprob <- prob

##          ## elas only included in some models, so check whether model model has slot
##          if (.hasSlot(model, "elas")) { 
##              for (j in 1:length(model@categories)) {
##                  ix <- map0.vals %in% model@categories[j]
##                  tprob[ix,j] <- tprob[ix,j] + model@elas[j] ## add elasticity
##              }
##          }
                  
##          if (!is.null(model@neighb)) {
##              nb.allow <- allowNeighb(x=model@neighb, cells=cells, categories=model@categories, rules=model@nb.rules)
##              tprob <- tprob * nb.allow ## neighbourhood decision rules
##          }
                  
##          ## implement other decision rules
##          if (!is.null(model@rules)) {
##              cd <- d - model@demand[i,] ## change direction
##              allow <- allow(x=map0.vals, hist=hist.vals, categories=model@categories, cd=cd,rules=model@rules)
##              tprob <- tprob * allow
##          }

##          ## make automatic conversions if necessary
##          auto <- .autoConvert(x=map0.vals, mask=mask.vals, prob=tprob, categories=model@categories)
##          map0.vals[auto$ix] <- auto$vals
##          tprob[auto$ix,] <- NA
                  
##          ## allocation
##          args <- c(list(tprob=tprob, map0.vals=map0.vals, demand=d, categories=model@categories), model@params)
##          map1.vals <- do.call(fun, args)
##          map1 <- raster::raster(map0, ...) 
##          map1[cells] <- map1.vals
##          maps <- raster::stack(maps, map1)
    
##          ## prepare model for next timestep
##          if (i < nrow(model@demand)) {
##              hist.vals <- .updatehist(map0.vals, map1.vals, hist.vals) ## update
##              map0.vals <- map1.vals 
##              if (!is.null(model@neighb)) model@neighb <- NeighbRasterStack(x=map1, neighb=model@neighb)
##          }
##      }    
##      out <- maps              
## }


