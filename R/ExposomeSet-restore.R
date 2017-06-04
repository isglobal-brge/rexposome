#' @describeIn ExposomeSet Returns the object at the same point is was
#' when created.
setMethod(
    f = "restore",
    signature = "ExposomeSet",
    definition = function(object) {
        exposome <- new("ExposomeSet",
            assayData = assayDataNew("environment",
                                     raw = assayDataElement(object, "raw"),
                                     exp = assayDataElement(object, "raw")
            ),
            phenoData = phenoData(object),
            featureData = featureData(object)
        )

        fData(exposome)$`.fct` <- ""
        fData(exposome)$`.trn` <- ""
        fData(exposome)$`.std` <- ""
        fData(exposome)$`.imp` <- ""

        validObject(exposome)

        return(exposome)
    }
)
