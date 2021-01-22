setMethod(
    f = "fData",
    signature="imExposomeSet",
    definition = function(object) {
        return(object@featureData)
    }
)
