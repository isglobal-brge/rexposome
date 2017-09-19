#' @describeIn imExposomeSet Method to obtain samples' names
setMethod(
    f = "sampleNames",
    signature = "imExposomeSet",
    definition = function(object) {
        rownames(psygenet2r::extract(object, rid = 1))
    }
)
