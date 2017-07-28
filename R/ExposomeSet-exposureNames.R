#' @describeIn ExposomeSet Getter to obtain the exposures's names.
setMethod(
  f = "exposureNames",
  signature = "ExposomeSet",
  definition = function(object) {
    rownames(assayData(object)[["exp"]])
  }
)
