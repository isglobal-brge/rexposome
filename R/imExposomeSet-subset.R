#' @describeIn imExposomeSet Get an \code{ExposomeSet} with the selected
#' imputation
#' @aliases imExposomeSet-methods
# @param x \code{imExposomeSet}
# @param i \code{numeric} with the number of imputaion to be returned
setMethod(
    f = "[[",
    signature = "imExposomeSet",
    definition = function(x, i) {
        toES(x, i)
    }
)

#' @describeIn imExposomeSet Subset an \code{imExposomeSet}
#' @aliases imExposomeSet-methods [
#' @param i Character corresponding to selected sample names.
#' @param j Character coresponding to selected exposures.
#' @param drop NOT USED
#' @param k Character corresponding to selected phenotypes.
#' @note Sample order is not guarantee
setMethod(
    f = "[",
    signature = "imExposomeSet",
    definition = function(x, i, j, k, ..., drop = FALSE) {
        if(!missing(i)) { # Subset samples
            if(class(i) %in% c("numeric", "integer")) {
                i <- sampleNames(x)[i]
            }
            if(sum(i %in% sampleNames(x)) != length(i)) {
                stop("Given samples not in imExposomeSet.")
            }
            x@assayData <- x@assayData[x@assayData$`.id` %in% i, ]
            x@phenoData <- x@phenoData[x@phenoData$`.id` %in% i, ]
        }
        if(!missing(j)) {
            if(class(j) %in% c("numeric", "integer")) {
                j <- exposureNames(x)[j]
            }
            if(sum(j %in% exposureNames(x)) != length(j)) {
                stop("Given exposures not in imExposomeSet.")
            }
            x@assayData <- x@assayData[ , c(".imp", ".id", j)]
            x@featureData <- x@featureData[j, ]
        }
        if(!missing(k)) {
            message(class(k))
            if(class(k) %in% c("numeric", "integer")) {
                k <- phenotypeNames(x)[k]
            }
            if(sum(k %in% phenotypeNames(x)) != length(k)) {
                stop("Given enotypes not in imExposomeSet.")
            }
            x@phenoData <- x@phenoData[ , c(".imp", ".id", k)]
        }

        validObject(x)
        return(x)
    }
)
