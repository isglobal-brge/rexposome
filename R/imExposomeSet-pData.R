setMethod(
    f = "pData",
    signature="imExposomeSet",
    definition = function(object) {
        return(object@phenoData)
    }
)
