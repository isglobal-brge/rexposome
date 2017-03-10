#' @describeIn imExposomeSet Getter to obtain the families's names of the
#' family of each exposure.
#' @param by.exposure If set to \code{TRUE} ir returns the family which
#' each exposure belongs
setMethod(
    f = "familyNames",
    signature = "imExposomeSet",
    definition = function(object, by.exposure = FALSE) {
        if(by.exposure) {
            fm <- as.character(fData(object)$Family)
            names(fm) <- fData(object)$Exposure
            return(fm)
        } else {
            unique(as.character(fData(object)$Family))
        }
    }
)
