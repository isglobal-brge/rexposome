setMethod(
    f = "show",
    signature="imExposomeSet",
    definition = function(object) {
        cat("Object of class 'imExposomeSet'\n", sep="")
        cat(" . exposures description:\n")
        cat("    . categorical: ", sum(fData(object)$`_type` == "factor"), "\n")
        cat("    . continuous: ", sum(fData(object)$`_type` == "numeric"), "\n")

        adim <- dim(object)
        cat(" . assayData:", adim[[1]], "exposures", adim[[2]], "individuals\n")

        adim <- dim(pData(object)) - 2
        cat(" . phenoData:", adim[[1]], "individuals", adim[[2]], "phenotypes\n")

        adim <- dim(fData(object)) - 2
        cat(" . featureData:", adim[[1]], "exposures", adim[[2]], "explanations\n")
    }
)
