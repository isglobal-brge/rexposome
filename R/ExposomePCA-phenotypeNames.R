setMethod(
    f = "phenotypeNames",
    signature = "ExposomePCA",
    definition = function(object) {
        colnames(pData(object))
    }
)
