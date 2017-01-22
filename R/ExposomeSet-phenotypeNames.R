setMethod(
  f = "phenotypeNames",
  signature = "ExposomeSet",
  definition = function(object) {
    return(varLabels(object@phenoData))
  }
)
