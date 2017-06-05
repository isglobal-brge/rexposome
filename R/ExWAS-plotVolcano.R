#' @describeIn ExWAS Method to plot a volcano plot for association between
#' exposures and phenotypes
#' @param p.value (default \code{-log10(0.001)}) Is the threshold from where
#' the exposures can be taken as significants.
#' @param show.effect (default \code{false}) Apply \code{exp} to obtained
#' beta.
setMethod(
    f = "plotVolcano",
    signature = "ExWAS",
    definition = function(x, p.value = -log10(0.001), show.effect = FALSE) {
        x <- psygenet2r::extract(x)
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
