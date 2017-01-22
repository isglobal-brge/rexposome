setMethod(
    f = "exposureNames",
    signature="ExposomePCA",
    definition=function(object) {
        featureNames(object)
    }
)
