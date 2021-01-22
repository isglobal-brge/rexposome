#' @describeIn ExposomeSet Getter to obtain the phenotypes's names.
setMethod(
  f = "phenotypeNames",
  signature = "ExposomeSet",
  definition = function(object) {
    return(varLabels(object@phenoData))
  }
)
