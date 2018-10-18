#' @describeIn imExposomeSet Method to obtain samples' names
setMethod(
    f = "exposureNames",
    signature = "imExposomeSet",
    definition = function(object) {
        colnames(extract(object, rid = 1))
    }
)
