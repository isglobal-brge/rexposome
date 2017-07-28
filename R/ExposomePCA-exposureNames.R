#' @describeIn ExposomePCA Getter to obtain the exposures's names.
setMethod(
    f = "exposureNames",
    signature="ExposomePCA",
    definition=function(object) {
        featureNames(object)
    }
)
