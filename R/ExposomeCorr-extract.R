#' @describeIn ExposomeCorr Return the raw correlation matrix
#' @param sort NOT USED
setMethod(
    f = "extract",
    signature = "ExposomeCorr",
    definition = function(object, sort, ...) {
        assayDataElement(object, "corr")
    }
)
