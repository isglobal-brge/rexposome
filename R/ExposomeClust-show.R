setMethod(
  f = "show",
  signature = "ExposomeClust",
  definition = function(object) {
    cat("Object of class 'ExposomeClust' (storageMode: ", storageMode(object), ")\n", sep="")

    cat(" . Method: ....", object@method, "\n")

    adim <- dim(object)
    cat(" . assayData:", adim[[1]], "exposures", adim[[2]], "samples\n")
    cat("    . element names:", paste(assayDataElementNames(object), collapse=", "), "\n")
    cat("    . exposures:", .wrpvec(rownames(assayData(object)[["exp"]])), "\n")
    cat("    . samples:", .wrpvec(colnames(assayData(object)[["exp"]])), "\n")


    adim <- dim(featureData(object))
    fte <- featureData(object)
    cat(" . featureData:", adim[[1]], "exposures", adim[[2]], "explanations\n")
    cat("    . exposures:", .wrpvec(rownames(pData(featureData(object)))), "\n")
    cat("    . descriptions:", .wrpvec(colnames(pData(featureData(object)))), "\n")

#     mod <- object@model[[1]]
#     tryCatch({
#       if (length(object@view.method) == 0) {
#         cat(" . #Cluster: ..", length(unique(mod$classification)), "\n")
#       } else {
#         cat(" . #Cluster: ..", length(unique(object@view.method$fun(mod))), "\n")
#       }
#     }, error=function(err) {
#       cat(" . #Cluster: ..", "NOT AVAILABLE\n")
#     })
    cat("    . #Cluster:", length(unique(classification(object))), "\n")
  }
)
