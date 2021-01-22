#' @describeIn imExposomeSet Method to obtain samples' names
setMethod(
    f = "sampleNames",
    signature = "imExposomeSet",
    definition = function(object) {
        rownames(extract(object, rid = 1))
    }
)
