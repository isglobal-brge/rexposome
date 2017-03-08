#' @describeIn ExposomeSet Returns the number of exsures, samples and phenotypes.
#' @param x An \code{\link{ExposomeSet}} object.
setMethod(
  f = "dim",
  signature = "ExposomeSet",
  definition = function(x) {
    nexp <- nrow(assayData(x)[["exp"]])
    nsam <- ncol(assayData(x)[["exp"]])
    nphe <- ncol(pData(x))
    x <- c(nexp, nsam, nphe)
    names(x) <- c("exposures", "samples", "phenotyes")
    return(x)
  }
)
