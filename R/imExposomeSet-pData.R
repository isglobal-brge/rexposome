setMethod(
    f = "pData",
    signature = "imExposomeSet",
    definition = function(object) {
        return(object@phenoData)
    }
)


setReplaceMethod(
    f = "pData",
    signature = signature("imExposomeSet", "data.frame"),
    definition = function(object, value) {
        object@phenoData <- value
        object
})
