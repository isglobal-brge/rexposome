#' @describeIn ExposomeClust Wrapper for \code{plotClassification} method.
#' @param x Object of class \code{ExposomeClust}
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("ExposomeClust", "ANY"),
    definition = function(x, y, ...) {
        plotClassification(x, ...)
    }
)

#' @describeIn ExposomeCorr Wrapper for \code{plotClassification} method.
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("ExposomeCorr", "ANY"),
    definition = function(x, y, ...) {
        plotCorrelation(x, ...)
    }
)

#' @describeIn ExposomePCA Wrapper for \code{plotPCA} method.
#' @param x Object of class \code{ExposomePCA}
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("ExposomePCA", "ANY"),
    definition = function(x, y, ...) {
        plotPCA(x, ...)
    }
)

#' @describeIn ExposomeSet Wrapper for \code{plotFamily} method.
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("ExposomeSet", "ANY"),
    definition = function(x, y, ...) {
        plotFamily(x, ...)
    }
)

#' @describeIn ExWAS Wrapper for \code{plotExwas} method.
#' @param x Object of class \code{ExWAS}
setMethod(
    f = "plot",
    signature = c("ExWAS", "ANY"),
    definition = function(x, y, ...) {
        plotExwas(x, ...)
    }
)

#' @describeIn imExposomeSet Wrapper for \code{plotFamily} method.
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("imExposomeSet", "ANY"),
    definition = function(x, y, ...) {
        plotFamily(x, ...)
    }
)

#' @describeIn mExWAS Wrapper for \code{plotExwas} method.
#' @param x Object of class \code{mExWAS}
#' @param y NOT USED
setMethod(
    f = "plot",
    signature = c("mExWAS", "ANY"),
    definition = function(x, y, ...) {
        plotExwas(x, ...)
    }
)
