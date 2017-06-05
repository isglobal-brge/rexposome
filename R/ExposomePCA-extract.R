#' @describeIn ExposomePCA Method to extract the raw results of the PCA.
#' @param table Can takes values \code{"exposures"} or \code{"individuals"}.
#' @param ... NOT USED
setMethod(
    f = "extract",
    signature="ExposomePCA",
    definition=function(object, table="exposures", ...) {
        table <- match.arg(table, choices=c("exposures", "individuals",
                                            "correlation", "eigen"))
        if(table=="exposures") {
            data.frame(object@pca$var$coord)
        } else if (table=="individuals") {
            data.frame(object@pca$ind$coord)
        } else if (table=="correlation"){
            data.frame(object@pca$var$cor)
        } else {
            data.frame(object@pca$eig)
        }
    }
)
