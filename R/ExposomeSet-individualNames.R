setMethod(
  f = "individualNames",
  signature = "ExposomeSet",
  definition = function(object) {
    colnames(assayData(object)[["exp"]])
  }
)

setReplaceMethod(
    f = "individualNames",
    signature = "ExposomeSet",
    definition = function(object, value) {
        colnames(assayData(object)[["exp"]]) <- value
        rownames(pData(object)) <- value
        pData(object)$id <- value
        prd <- protocolData(object)
        if (nrow(prd) == 0) {
            prd <- pd[,integer(0)]
        } else {
            individualNames(prd) <- value
        }
        object@protocolData <- prd
        ## unsafeSetSlot(object, "assayData", ad) ## bastards
        object
    }
)
