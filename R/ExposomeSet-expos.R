#' @describeIn ExposomeSet Returns a \code{data.frame} with exposures.
setMethod(
    f = "expos",
    signature = "ExposomeSet",
    definition = function(object) {
        ms <- data.frame(t(object@assayData$exp))
        if( !".type" %in% colnames(fData(object))) {
            stop("Invalid 'ExposomeSet'. It has no '.type' column in its 'fData'.")
        }
        type <- fData(object)[colnames(ms), ".type"]
        expo.n <- rownames(fData(object))
        names(type) <- expo.n
        for(ii in expo.n) {
            if(type[ii] == "numeric") {
                ms[ , ii] <- as.numeric( as.character( ms[ , ii]) )
            } else if(type[ii] == "factor") {
                ms[ , ii] <- as.factor(ms[ , ii])
            }
        }
        ms
    }
)
