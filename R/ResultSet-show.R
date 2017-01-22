setMethod(
    f = "show",
    signature="ResultSet",
    definition = function(object) {
        cat("Object of class 'ResultSet'\n", sep="")
        cat(" . created with:", object@fun_origin, "\n")
        cat(" . original input:", paste(object@class_origin, collapse = ", "), "\n")
        cat("    . names:", paste(object@names, collapse = ", "), "\n")
        cat(" . #results:", length(object@results), "\n")

        cat(" . featureData: ", length(object@fData), "\n")
        for(nm in names(object@fData)) {
            nr <- nrow(object@fData[[nm]])
            nc <- ncol(object@fData[[nm]])
            cat("    . ", nm, ": ", nr, "x", nc, "\n", sep="")
        }
    }
)
