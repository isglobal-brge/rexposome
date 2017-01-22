setMethod(
  f = "classification",
  signature = "ExposomeClust",
  definition = function(object) {
    c <- pData(object)$cluster
    names(c) <- rownames(pData(object))
    return(c)
  }
)
