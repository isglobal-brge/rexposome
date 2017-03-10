#' @describeIn imExposomeSet Method to obtain samples' names
setMethod(
    f = "sampleNames",
    signature = "imExposomeSet",
    definition = function(object) {
        unique(expos(object)[ , 2])
    }
)
