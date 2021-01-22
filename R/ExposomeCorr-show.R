setMethod(
  f = "show",
  signature = "ExposomeCorr",
  definition = function(object) {
    cat("Object of class 'Exposome Correlation' (storageMode: ", storageMode(object), ")\n", sep="")

    adim <- dim(object)
    cat(" . assayData:", adim[[1]], "x", adim[[2]], "exposure-correlations\n")
    cat("    . element names:", paste(assayDataElementNames(object), collapse=", "), "\n")
    cat("    . exposures:", .wrpvec(rownames(assayData(object)[["corr"]])), "\n")

    adim <- dim(featureData(object))
    fte <- featureData(object)
    cat(" . featureData:", adim[[1]], "exposures", adim[[2]], "explanations\n")
    cat("    . exposures:", .wrpvec(rownames(pData(featureData(object)))), "\n")
    cat("    . descriptions:", .wrpvec(colnames(pData(featureData(object)))), "\n")
  }
)
