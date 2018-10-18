#' @describeIn ExWAS Method to obtain the Threshold for effective tests (TEF)
setMethod(
    f = "names",
    signature = "ExWAS",
    definition = function(x) {
        if(nrow(x@comparison) > 0) {
            unique(sapply(strsplit(rownames(x@comparison), "\\$"), "[[", 1))
        } else {
            character()
        }
    }
)
