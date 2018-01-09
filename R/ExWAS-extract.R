#' @describeIn ExWAS Method to obtain the matrix of association scores
#' @param sort If \code{TRUE}, the results are ordered by pvalue.
#' @param ... NOT USED
setMethod(
  f = "extract",
  signature = "ExWAS",
  definition = function(object, sort = TRUE, ...) {
    x <- object@comparison
    if(sort) {
        x <- x[order(x$pvalue), c("pvalue", "effect", "X2.5", "X97.5")]
    } else {
        x <- x[order(rownames(x)), c("pvalue", "effect", "X2.5", "X97.5")]
    }
    x$pvalue <- as.numeric(as.character(x$pvalue))
    x$effect <- as.numeric(as.character(x$effect))
    x$X2.5 <- as.numeric(as.character(x$X2.5))
    x$X97.5 <- as.numeric(as.character(x$X97.5))
    x
  }
)
