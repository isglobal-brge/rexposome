setMethod(
  f = "exposureNames",
  signature = "ExposomeSet",
  definition = function(object) {
    rownames(assayData(object)[["exp"]])
  }
)
