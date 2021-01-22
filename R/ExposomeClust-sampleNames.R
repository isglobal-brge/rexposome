#' @describeIn ExposomeClust Method to obtain samples' names
#' @param object An object of class \link{ExposomeClust}
setMethod(
  f = "sampleNames",
  signature = "ExposomeClust",
  definition = function(object) {
    colnames(assayData(object)[["exp"]])
  }
)
