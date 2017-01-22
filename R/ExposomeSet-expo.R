setMethod(
    f = "expos",
    signature = "ExposomeSet",
    definition = function(object) {
        t(assayDataElement(object, "exp"))
    }
)
