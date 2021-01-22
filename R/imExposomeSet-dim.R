#' @describeIn imExposomeSet Returns the number of exsures, samples and phenotypes.
#' @param x An \code{\link{imExposomeSet}} object.
setMethod(
    f = "dim",
    signature = "imExposomeSet",
    definition = function(x) {
        nexp <- nrow(x@featureData)
        nsam <- sum(x@assayData[ , 1] == 0)
        nphe <- length(colnames(x@phenoData)) - 2
        x <- c(nexp, nsam, nphe)
        names(x) <- c("exposures", "samples", "phenotyes")
        return(x)
    }
)
