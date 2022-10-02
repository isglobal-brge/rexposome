#' @describeIn ExWAS Method to obtain the matrix of association scores
#' @param sort If \code{TRUE}, the results are ordered by pvalue.
#' @param ... NOT USED
setMethod(
  f = "get_robust_sd",
  signature = "ExWAS",
  definition = function(object, sort = TRUE, ...) {
    data.frame(object@robust.std.err)
  }
)
