setMethod(
    f = "exposureNames",
    signature="ResultSet",
    definition = function(object) {
        return(names(object@results))
    }
)
