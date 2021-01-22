# Internal function  \code{.pheno_type}
#
# Function to get the values of a given phenotype or to get its type
# (numeric or factor.)
#
# @param object An \code{ExposomeSet}.
# @param pheno The name of the exposure to be queryed.
# @param as.type By default \code{FALSE}. If \code{TRUE} returns the exposure
# formated as \code{numeric} or as \code{factor}.
# @return A character with \code{"numeric"} or \code{"factor"} or a vector
# with the exposure's value as \code{mumeric} or as \code{factor}
.pheno_type <- function(object, pheno, exp2fac = 5, as.type = FALSE) {
  phenotypes <- pData(phenoData(object))[ , pheno]
  phenotypes.u <- unique(as.character(phenotypes))
  if(!as.type) {
    if(length(phenotypes.u) >= exp2fac) {
      tryCatch({
        as.numeric(as.character(phenotypes.u))
        return("numeric")
      }, warning = function(w) {
        return("factor")
      }, error = function(e) {
        return("factor")
      })
    } else {
      return("factor")
    }
  } else {
    if(length(phenotypes) >= exp2fac) {
      return(phenotypes)
    } else {
      return(as.factor(phenotypes))
    }
  }
}
