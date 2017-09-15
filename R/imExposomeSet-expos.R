#' @describeIn imExposomeSet Returns a \code{data.frame} with exposures.
setMethod(
    f = "expos",
    signature="imExposomeSet",
    definition = function(object) {
        ms <- data.frame(object@assayData)
        type <- fData(object)[colnames(ms)[-(1:2)], ".type"]
        expo.n <- rownames(fData(object))
        names(type) <- expo.n
        for(ii in expo.n) {
            if(type[ii] == "numeric") {
                ms[ , ii] <- as.numeric(ms[ , ii])
            } else if(type[ii] == "factor") {
                ms[ , ii] <- as.factor(ms[ , ii])
            }
        }
        ms
    }
)
