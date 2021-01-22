#' @describeIn ExWAS Method to plot a volcano plot for association between
#' exposures and phenotypes
#' @param p.value (default \code{-log10(0.001)}) Is the threshold from where
#' the exposures can be taken as significants.
#' @param labels (optional) Character vector with the labels for each exposure.
#' It must be labeled vector.
#' @param show.effect (default \code{false}) Apply \code{exp} to obtained
#' beta.
setMethod(
    f = "plotVolcano",
    signature = "ExWAS",
    definition = function(x, p.value = -log10(0.001), labels, show.effect = FALSE) {
        if(missing(labels)) {
            lbl <- names(x)
        } else {
            lbl <- sapply(strsplit(names(x), "\\$"), function(nm) {
                ex <- ifelse(nm[1] %in% names(labels), labels[nm[1]], nm[1])
                if( length( nm ) == 2 ) {
                    ex <- paste0(ex, " (", nm[2], ")")
                }
                ex
            })
        }
        x <- extract(x)
        volcano_plot(
            pval = x$pvalue,
            fc = x$effect,
            names = lbl,
            tFC = NULL,
            tPV = p.value,
            show.effect = show.effect
        ) + ggplot2::xlab("effect")
    }
)
