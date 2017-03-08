#' @describeIn ExposomeCorr Draws both a matrix of circos plot of correlations
#' @param object An \code{\link{ExposomeCorr}} object.
#' @param type To choose between \code{"circos"} and \code{"matrix"}.
#' @param ... Arguments passed to \code{corrplot} when \code{type="matrix"}.
setMethod(
  f = "plotCorrelation",
  signature = "ExposomeCorr",
  definition = function(object, type = c("circos", "matrix"), ...) {
    type <- match.arg(type)

    desc <- pData(featureData(object))
    colnames(desc)[1] <- "Family"
    desc <- desc[order(desc$Family), ]

    if (type == "matrix") {
      .correlation_matrix_plot(assayData(object)[["corr"]], desc, ...)
    } else if (type == "circos") {
      .correlation_circos_plot(assayData(object)[["corr"]], desc, ...)
    } else {
      stop("Invalid given plot 'type'.")
    }
  }
)

