setMethod(
    f = "length",
    signature="ResultSet",
    definition = function(x) {
        return(length(x@results))
    }
)
