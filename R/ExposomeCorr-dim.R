#' @describeIn ExposomeCorr Return the dimension of the internat matrix of
#' correlation.
setMethod(
  f = "dim",
  signature = "ExposomeCorr",
  definition = function(x) {
    dim(assayData(x)[["corr"]])
  }
)
