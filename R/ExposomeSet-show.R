setMethod(
  f = "show",
  signature="ExposomeSet",
  definition = function(object) {
    cat("Object of class 'ExposomeSet' (storageMode: ", storageMode(object), ")\n", sep="")
    cat(" . exposures description:\n")
    cat("    . categorical: ", sum(fData(object)$`.type` == "factor"), "\n")
    cat("    . continuous: ", sum(fData(object)$`.type` == "numeric"), "\n")

    cat(" . exposures transformation:\n")
    cat("    . categorical:", sum(fData(object)$`.fct` != ""), "\n")
    cat("    . transformed:", sum(fData(object)$`.trn` != ""), "\n")
    cat("    . standardized:", sum(fData(object)$`.std` != ""), "\n")
    cat("    . imputed:", sum(fData(object)$`.imp` != ""), "\n")

    adim <- dim(object)
    cat(" . assayData:", adim[[1]], "exposures", adim[[2]], "individuals\n")
    cat("    . element names:", paste(assayDataElementNames(object), collapse=", "), "\n")
    cat("    . exposures:", .wrpvec(rownames(assayData(object)[["exp"]])), "\n")
    cat("    . individuals:", .wrpvec(colnames(assayData(object)[["exp"]])), "\n")

    adim <- dim(phenoData(object))
    phe <- phenoData(object)
    cat(" . phenoData:", adim[[1]], "individuals", adim[[2]], "phenotypes\n")
    cat("    . individuals:", .wrpvec(rownames(pData(phenoData(object)))), "\n")
    cat("    . phenotypes:", .wrpvec(colnames(pData(phenoData(object)))), "\n")

    adim <- dim(featureData(object))
    fte <- featureData(object)
    cat(" . featureData:", adim[[1]], "exposures", adim[[2]], "explanations\n")
    cat("    . exposures:", .wrpvec(rownames(pData(featureData(object)))), "\n")
    cat("    . descriptions:", .wrpvec(colnames(pData(featureData(object)))), "\n")

   cat("experimentData: use 'experimentData(object)'\n")
   pmids <- pubMedIds(object)
   if (length(pmids) > 0 && all(pmids != "")) {
     cat("  pubMedIds:", paste(pmids, sep=", "), "\n")
   }
   cat("Annotation:", annotation(object), "\n")
  }
)
