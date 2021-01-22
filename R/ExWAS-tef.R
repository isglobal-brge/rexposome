#' @describeIn ExWAS Method to obtain the Threshold for effective tests (TEF)
setMethod(
    f = "tef",
    signature = "ExWAS",
    definition = function(object) {
        return(object@effective)
    }
)
