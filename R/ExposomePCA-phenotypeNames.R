#' @describeIn ExposomePCA Getter to obtain the phenotype's names.
setMethod(
    f = "phenotypeNames",
    signature = "ExposomePCA",
    definition = function(object) {
        colnames(pData(object))
    }
)
