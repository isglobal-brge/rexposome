#' @describeIn ExposomeCorr Return the raw correlation matrix
setMethod(
    f = "extract",
    signature = "ExposomeCorr",
    definition = function(object) {
        assayDataElement(object, "corr")
    }
)
