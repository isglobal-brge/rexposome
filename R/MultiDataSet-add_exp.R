setMethod(
    f = "add_exp",
    signature = c("MultiDataSet", "ExposomeSet"),
    definition = function(object, expoSet, ...) {
        object <- MultiDataSet::add_eset(object, expoSet,
                                         dataset.type = "exposures",
                                         GRanges = NA, ...)
        return(object)
    }
)
