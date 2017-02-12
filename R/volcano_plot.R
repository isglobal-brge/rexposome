#' Function to draw a Volcano Plot
#'
#' Function that takes two numeric vectors (P.Value and fold change)
#' and draws a volcano plot using \link{ggplot2}
#'
#' @param pval numeric vector of P.Values
#' @param pc
#' @return A \code{ggplot} object
volcano_plot <- function(pval, fc) {
    dta <- data.frame(P.Value=pval, FC=fc, cl="gray87", alp=0.2, stringsAsFactors=FALSE)
    dta$PV <- -log10(dta$P.Value)
    dta$feature <- rownames(dta)

    dta$clr[dta$PV >= tPV] <- "tan3"
    dta$alp[dta$PV >= tPV] <- 0.4
    dta$clr[abs(dta$FC) >= tFC] <- "olivedrab"
    dta$alp[abs(dta$FC) >= tFC] <- 0.4

    dta$clr[dta$PV >= tPV & abs(dta$FC) >= tFC] <- "lightskyblue"
    dta$alp[dta$PV >= tPV & abs(dta$FC) >= tFC] <- 0.9

    clrvalues <- c("gray87", "tan3", "olivedrab", "lightskyblue")
    names(clrvalues) <- c("gray87", "tan3", "olivedrab", "lightskyblue")

    plt <- ggplot2::ggplot(dta, ggplot2::aes(x=FC, y=PV, color=clr, fill=clr, alpha=alp)) +
        ggplot2::theme_bw() +
        ggplot2::geom_point() +
        ggplot2::scale_colour_manual(values=clrvalues) +
        ggplot2::xlab("Log Fold Change") +
        ggplot2::ylab("-log10(P.Value)") +
        ggplot2::theme(legend.position="none") +
        ggrepel::geom_text_repel(
            data = subset(dta, dta$PV >= tPV & abs(dta$FC) >= tFC),
            ggplot2::aes(FC, PV, label=feature),
            size = 2,
            box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"),
            color="black"
        )

    if(sum(dta$PV >= tPV) > 0) {
        plt <- plt + ggplot2::geom_hline(yintercept=tPV, linetype="dotdash", color="gray69", size=0.75)
    }

    if(sum(dta$FC <= -tFC) > 0) {
        plt <- plt + ggplot2::geom_vline(xintercept=-tFC, linetype="dotdash", color="gray69", size=0.75)
    }

    if(sum(dta$FC >= tFC) > 0) {
        plt <- plt + ggplot2::geom_vline(xintercept=tFC, linetype="dotdash", color="gray69", size=0.75)
    }
}
