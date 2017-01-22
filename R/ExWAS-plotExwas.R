#' @describeIn ExWAS Method to plot a manhatan plot for association between
#' exposures and phenitypes
#' @aliases ExWAS-methods
setMethod(
    f = "plotExwas",
    signature = "ExWAS",
    definition = function(object, color, show.effective = TRUE) {
        tbl <- extract(object)
        tbl$lpv <- -log10(tbl$pvalue)
        tbl$exposure <- rownames(tbl)
        tbl$family <- object@description[rownames(tbl), 1]

        nm <- unique(as.character(tbl$family))
        if(missing(color)) {
            colorPlte <- sample(rainbow(length(nm)))
            names(colorPlte) <- nm
        } else {
            colorPlte <- color
        }

        tbl <- tbl[order(tbl$family, tbl$exposure), ]
        tbl$exposure <- factor(tbl$exposure, levels = tbl$exposure)

        plt <- ggplot2::ggplot(tbl, ggplot2::aes(x = lpv, y = exposure, color = family)) +
            ggplot2::geom_point() +
            ggplot2::theme_minimal() +
            ggplot2::theme(panel.spacing = ggplot2::unit(0.5, 'lines'), strip.text.y = ggplot2::element_text(angle = 0)) +
            ggplot2::ylab("") +
            ggplot2::xlab(expression(-log10(pvalue))) +
            ggplot2::labs(colour="Exposure's Families") +
            ggplot2::scale_color_manual(breaks = names(colorPlte),
                                        values = colorPlte)
        if(show.effective) {
            plt <- plt + ggplot2::geom_vline(xintercept = -log10(object@effective), colour="Brown")
        }

        return(plt)

})
