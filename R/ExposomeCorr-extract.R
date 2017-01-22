setMethod(
    f = "extract",
    signature = "ExposomeCorr",
    definition = function(object) {
        assayDataElement(object, "corr")
    }
)
