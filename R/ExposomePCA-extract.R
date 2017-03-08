#' @describeIn ExposomePCA Method to extract the raw results of the PCA.
#' @param table Can takes values \code{"exposures"} or \code{"individuals"}.
setMethod(
    f = "extract",
    signature="ExposomePCA",
    definition=function(object, table="exposures", ...) {
        table <- match.arg(table, choices=c("exposures", "individuals"))
        if(table=="exposures") {
            data.frame(object@pca$var$coord)
        } else {
            data.frame(object@pca$ind$coord)
        }
    }
)
