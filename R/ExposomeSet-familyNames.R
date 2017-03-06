#' @describeIn ExposomeSet Getter to obtain the families's names of the
#' family of each exposure.
setMethod(
  f = "familyNames",
  signature = "ExposomeSet",
  definition = function(object, by.exposure = FALSE) {
    if(by.exposure) {
      fm <- as.character(pData(featureData(object))[ , 1])
      names(fm) <- rownames(pData(featureData(object)))
      return(fm)
    } else {
      return(as.character(unique(pData(featureData(object))[ , 1])))
    }
  }
)
