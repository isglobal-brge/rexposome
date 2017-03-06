#' @describeIn ExposomeCorr Draws both a matrix of circos plot of correlations
setMethod(
  f = "plotCorrelation",
  signature = "ExposomeCorr",
  definition = function(x, type = c("circos", "matrix"), ...) {
    type <- match.arg(type)

    desc <- pData(featureData(x))
    colnames(desc)[1] <- "Family"
    desc <- desc[order(desc$Family), ]

    if (type == "matrix") {
      .correlation_matrix_plot(assayData(x)[["corr"]], desc, ...)
    } else if (type == "circos") {
      .correlation_circos_plot(assayData(x)[["corr"]], desc, ...)
    } else {
      stop("Invalid given plot 'type'.")
    }
  }
)

