setMethod(
    f = "show",
    signature="ExposomePCA",
    definition = function(object) {
        cat("Object of class 'ExposomePCA' (storageMode: ", storageMode(object), ")\n", sep="")
        cat(" . num. prin. components:", nrow(object@pca$eig), "\n")


        adim <- dim(object)
        cat(" . assayData:", adim[[1]], "exposures", adim[[2]], "samples\n")
        cat("    . element names:", paste(assayDataElementNames(object), collapse=", "), "\n")
        cat("    . exposures:", .wrpvec(rownames(assayData(object)[["exp"]])), "\n")
        cat("    . samples:", .wrpvec(colnames(assayData(object)[["exp"]])), "\n")

        adim <- dim(phenoData(object))
        phe <- phenoData(object)
        cat(" . phenoData:", adim[[1]], "samples", adim[[2]], "phenotypes\n")
        cat("    . samples:", .wrpvec(rownames(pData(phenoData(object)))), "\n")
        cat("    . phenotypes:", .wrpvec(colnames(pData(phenoData(object)))), "\n")

        adim <- dim(featureData(object))
        fte <- featureData(object)
        cat(" . featureData:", adim[[1]], "exposures", adim[[2]], "explanations\n")
        cat("    . exposures:", .wrpvec(rownames(pData(featureData(object)))), "\n")
        cat("    . descriptions:", .wrpvec(colnames(pData(featureData(object)))), "\n")

    }
)
