
#' @export
names.ExpVarRasterStack = function(x) x@index[,1,drop=TRUE]

#' @export
names.PerformanceList = function(x) x@labels

#' @export
names.PredictionList = function(x) x@labels

#' @export
names.PredictiveModelList = function(x) x@labels
