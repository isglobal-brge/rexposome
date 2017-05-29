#' @describeIn ExWAS Method to plot a manhatan plot for association between
#' exposures and phenitypes
#' @param object An object of class \code{ExWAS}, \code{mExWAS} or \code{nlExWAS}.
#' @param color (optional) A vector of colors. The vector must have length
#' equal to the number of families. The vector must be names with the
#' name of the families.
#' @param exp.order (optional) Order of the exposures.
#' @param show.effect (default FALSE) Applyes an exponential
#' transformation on the effects of the exposures.
setMethod(
    f = "plotVolcano",
    signature = "ExWAS",
    definition = function(x, p.value = -log10(0.001), show.effect = FALSE) {
        x <- extract(x)
        volcano_plot(
            pval = x$pvalue,
            fc = x$effect,
            names = rownames(x),
            tFC = NULL,
            tPV = p.value,
            show.effect = show.effect
        ) + ggplot2::xlab("effect")
    }
)
