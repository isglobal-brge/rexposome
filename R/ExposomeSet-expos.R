#' @describeIn ExposomeSet Returns a \code{data.frame} with exposures.
setMethod(
    f = "expos",
    signature = "ExposomeSet",
    definition = function(object) {
        ms <- data.frame(t(data.frame(object@assayData$exp)))
        if( !".type" %in% colnames(fData(object))) {
            stop("Invalid 'ExposomeSet'. It has no '.type' column in its 'fData'.")
        }
        type <- fData(object)[colnames(ms)[-(1:2)], ".type"]
        expo.n <- rownames(fData(object))[-(1:2)]
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
