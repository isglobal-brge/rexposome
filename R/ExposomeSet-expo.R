#' @describeIn ExposomeSet Returns a \code{data.frame} with exposures.
setMethod(
    f = "expos",
    signature = "ExposomeSet",
    definition = function(object) {
        ms <- data.frame(t(assayDataElement(object, "exp")))
        type <- fData(object)[colnames(ms), "_type"]
        for(ii in 1:length(type)) {
            if(type[ii] == "numeric") {
                ms[ , ii] <- as.numeric( as.character( ms[ , ii] ) )
            } else {
                ms[ , ii] <- as.factor(ms[ , ii])
            }
        }
        ms
    }
)

