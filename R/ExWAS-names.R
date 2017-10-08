#' @describeIn ExWAS Method to obtain the Threshold for effective tests (TEF)
setMethod(
    f = "names",
    signature = "ExWAS",
    definition = function(x) {
        unique(sapply(strsplit(rownames(sys@comparison), "\\$"), "[[", 1))
    }
)
