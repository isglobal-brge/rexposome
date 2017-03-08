#' @describeIn ExWAS Method to obtain the matrix of association scores
#' @param sort If \code{TRUE}, the results are ordered by pvalue.
#' @param ... NOT USED
setMethod(
  f = "extract",
  signature = "ExWAS",
  definition = function(object, sort = TRUE, ...) {
    x <- object@comparison
    if(sort) {
        x <- x[order(x$pvalue), c("pvalue", "effect", "2.5", "97.5")]
    }
    x
  }
)
