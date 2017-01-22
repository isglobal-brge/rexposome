setMethod(
    f = "featureData",
    signature = "ResultSet",
    definition = function(object) {
        return(object@fData)
    }
)

setMethod(
    f = "fData",
    signature = "ResultSet",
    definition = function(object) {
        return(lapply(object@fData, function(x) as(x, "data.frame")))
    }
)
