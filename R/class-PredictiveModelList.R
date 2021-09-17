#' Class PredictiveModelList
#'
#' An S4 class to hold multiple mathematical models for different land use
#' categories belonging to the same map.
#'
#' @slot models list of predictive models
#' @slot categories numeric vector of land use categories
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass PredictiveModelList
#' @rdname PredictiveModelList-class
setClass("PredictiveModelList",
         slots = c(
           models = "list",
           categories = "numeric",
           labels = "character"),
         validity = function(object) {
             check1 <- (length(object@models) == length(object@categories))
             if (!check1) stop("Number of models does not equal number of land use categories")
             return(TRUE)
         }
         )
