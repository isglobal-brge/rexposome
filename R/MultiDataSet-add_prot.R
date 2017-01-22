setMethod(
    f = "add_prot",
    signature = c("MultiDataSet", "ExpressionSet"),
    definition = function(object, expoSet, ...) {
        object <- MultiDataSet::add_eset(object, expoSet,
                                         dataset.type = "protein",
                                         GRanges = NA, ...)
        return(object)
    }
)
