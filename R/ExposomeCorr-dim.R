setMethod(
  f = "dim",
  signature = "ExposomeCorr",
  definition = function(x) {
    dim(assayData(x)[["corr"]])
  }
)
