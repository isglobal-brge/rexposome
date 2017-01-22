setMethod(
    f = "rid",
    signature = "ResultSet",
    definition = function(object) {
        return(names(object@results))
    }
)
