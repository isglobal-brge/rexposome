setMethod(
    f = "show",
    signature="imExposomeSet",
    definition = function(object) {
        cat("Object of class 'imExposomeSet'\n", sep="")
        cat(" . exposures description:\n")
        cat("    . categorical: ", sum(fData(object)$`.type` == "factor"), "\n")
        cat("    . continuous: ", sum(fData(object)$`.type` == "numeric"), "\n")

        ni <- unique(pData(object)$`.imp`)
        if(0 %in% ni) {
            m <- paste0(length(ni), " (raw detected)" )
        } else {
            m <- paste0(length(ni), " (no raw detected)" )
        }
        cat(" . #imputations:", m, "\n")

        adim <- dim(object)
        cat(" . assayData:", adim[[1]], "exposures", adim[[2]], "individuals\n")

        adim <- dim(pData(object))
        cat(" . phenoData:", adim[[1]] / length(ni), "individuals", adim[[2]], "phenotypes\n")

        adim <- dim(fData(object))
        cat(" . featureData:", adim[[1]], "exposures", adim[[2]] - 1, "explanations\n")
    }
)
