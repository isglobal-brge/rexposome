#' @describeIn imExposomeSet Method to obtain samples' names
setMethod(
    f = "exposureNames",
    signature = "imExposomeSet",
    definition = function(object) {
        colnames(expos(object))[-(1:2)]
    }
)
