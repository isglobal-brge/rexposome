#' @describeIn imExposomeSet Getter to obtain the families's names of the
#' family of each exposure.
setMethod(
    f = "phenotypeNames",
    signature = "imExposomeSet",
    definition = function(object) {
        colnames(pData(object))[-(1:2)]
    }
)
