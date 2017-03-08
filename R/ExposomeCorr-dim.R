#' @describeIn ExposomeCorr Return the dimension of the internat matrix of
#' correlation.
#' @param x Object of class \link{ExposomeCorr}
setMethod(
  f = "dim",
  signature = "ExposomeCorr",
  definition = function(x) {
    dim(assayData(x)[["corr"]])
  }
)
