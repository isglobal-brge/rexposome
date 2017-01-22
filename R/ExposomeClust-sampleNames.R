setMethod(
  f = "sampleNames",
  signature = "ExposomeClust",
  definition = function(object) {
    colnames(assayData(object)[["exp"]])
  }
)
