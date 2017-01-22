# Internal function  \code{.family_type}
#
# Function to get the values of a given family or to get its type
# (numeric or factor.)
#
# @param object An \code{ExposomeSet}.
# @param family The name of the family to be queryed.
# @param as.type By default \code{FALSE}. If \code{TRUE} returns the family's
# exposures formated as \code{numeric} or as \code{factor} in a data.frame.
# @return A character with \code{"numeric"} or \code{"factor"} or a data.frame
# with the exposures's value as \code{mumeric} or as \code{factor}
# @examples
# \dontrun{
# .family_type(x, "Indoor air")
# # return "factor"
# }
.family_type <- function(object, family, as.type = FALSE) {
  exposures <- rownames(pData(featureData(object)))[pData(featureData(object))[ , 1] == family]
  if(!as.type) {
    type <- unique(fData(object)[exposures, "_type"] )
    if(length(type) == 1) {
      return(type)
    } else {
      return("mix")
    }
  } else {
    dd <- as.data.frame(object, phe = FALSE)[ , exposures, drop = FALSE]
    colnames(dd) <- exposures
    rownames(dd) <- colnames(assayData(object)[["exp"]])
    type <- fData(object)[exposures, "_type"]
    for(ii in 1:length(type)) {
      dd[, ii] <- switch (type[ii],
        numeric = as.numeric(dd[, ii]),
        factor = as.factor(dd[, ii])
      )
    }
    return(dd)
  }
}
