#' @describeIn ExposomeSet Allows to add an \code{ExposomeSet} to a
#' \code{MultiDataSet}.
#' @param expoSet An \code{\link{ExposomeSet}} object
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
