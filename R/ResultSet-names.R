setMethod(
    f = "names",
    signature="ResultSet",
    definition = function(x) {
        return(names(x@fData))
    }
)
