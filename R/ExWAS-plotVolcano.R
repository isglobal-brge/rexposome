#' @describeIn ExWAS Method to plot a manhatan plot for association between
#' exposures and phenitypes
#' @param object An object of class \code{ExWAS}, \code{mExWAS} or \code{nlExWAS}.
#' @param color (optional) A vector of colors. The vector must have length
#' equal to the number of families. The vector must be names with the
#' name of the families.
#' @param exp.order (optional) Order of the exposures.
#' @param show.effective (default TRUE) draws a brown line on the
#' threshold given by the effective number of tests.
setMethod(
    f = "plotVolcano",
    signature = "ExWAS",
    definition = function(x, p.value = -log10(0.001)) {
        x <- extract(x)
        volcano_plot(
            pval = x$pvalue,
            fc = x$effect,
            names = rownames(x),
            tFC = NULL,
            tPV = p.value,
            show.effect = FALSE
        ) + ggplot2::xlab("effect")
    }
)
