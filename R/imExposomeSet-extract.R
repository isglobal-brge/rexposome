#' @describeIn imExposomeSet Method to extract exposures for a single imputation
#' @param rid Number of the imputation to be extracted
setMethod(
    f = "extract",
    signature="imExposomeSet",
    definition = function(object, rid=-1, ...) {
        if(rid > -1) {
            if(rid > object@nimputation) {
                stop("Given 'rid' with imputation number is too large.")
            }
            tt <- object@assayData[object@assayData[ , 1] == rid, -(1:2), drop=FALSE]
            rownames(tt) <- object@assayData[object@assayData[ , 1] == rid, ".id"]
            return(tt)
        } else {
            stop("Invalid given 'rid'.")
        }
    }
)
