#' Function to draw a Volcano Plot
#'
#' Function that takes two numeric vectors (P-Value and fold change)
#' and draws a volcano plot using \link{ggplot2}
#'
#' @param pval numeric vector of P.Values
#' @param fc numeric vector of fold change
#' @param names character vector with the feature's names.
#' @param size (default \code{2}) Sice of the labels in case they are
#' placed.
#' @param tFC (default \code{2}) fold change threshold. It can be set to
#' \code{NULL} to do not filter.
#' @param tPV (default \code{-log10(0.001)}) P-Value threshold. It can be set
#' to \code{NULL} to not filter.
#' @param show.effect (default \code{FALSE}) If set to \code{TRUE}, the X-axis
#' will should \code{2^logFC} instead to teh default \code{logFC}.
#' @return A \code{ggplot} object
#' @examples
#' data(exposome)
#' w1 <- extract(exwas(expo[1:20, ], asthma~1, family = "binomial"))
#' volcano_plot(w1$pvalue, w1$effect, rownames(w1))
#' @export
volcano_plot <- function(pval, fc, names, size=2, tFC=2, tPV=-log10(0.001),
                         show.effect=FALSE) {
    if(missing(names)) {
        names <- names(pval)
    }
    dta <- data.frame(P.Value=pval, FC=fc, names, clr="gray87", alp=0.5, stringsAsFactors=FALSE)
    dta$PV <- -log10(dta$P.Value)
    dta$feature <- rownames(dta)

    if(show.effect) {
        dta$FC <- 2^dta$FC
    }

    if(!is.null(tPV)) {
        dta$clr[dta$PV >= tPV] <- "tan3"
        dta$alp[dta$PV >= tPV] <- 0.7
    }
    if(!is.null(tFC)) {
        dta$clr[abs(dta$FC) >= tFC] <- "olivedrab"
        dta$alp[abs(dta$FC) >= tFC] <- 0.7
    }

    if(!is.null(tPV) & !is.null(tFC)) {
        dta$clr[dta$PV >= tPV & abs(dta$FC) >= tFC] <- "lightskyblue"
        dta$alp[dta$PV >= tPV & abs(dta$FC) >= tFC] <- 0.9
    }

    clrvalues <- c("gray87", "tan3", "olivedrab", "lightskyblue")
    names(clrvalues) <- c("gray87", "tan3", "olivedrab", "lightskyblue")

    plt <- ggplot2::ggplot(dta, ggplot2::aes_string(x="FC", y="PV", color="clr", fill="clr")) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(alpha=1) +
        ggplot2::scale_colour_manual(values=clrvalues) +
        ggplot2::ylab(expression(-log[10](P-Value))) +
        ggplot2::theme(legend.position="none")

    if(show.effect) {
        plt <- plt + ggplot2::xlab("effect")
    } else {
        plt <- plt + ggplot2::xlab(expression(log[2](Fold~~Change)))
    }

    if(!is.null(tPV) & !is.null(tFC)) {
        plt <- plt + ggrepel::geom_text_repel(
            data = subset(dta, dta$PV >= tPV & abs(dta$FC) >= tFC),
            ggplot2::aes_string("FC", "PV", label="names"),
            size = size,
            box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"),
            color="black"
        )
    } else if(!is.null(tPV) & is.null(tFC)) {
        plt <- plt + ggrepel::geom_text_repel(
            data = subset(dta, dta$PV >= tPV),
            ggplot2::aes_string("FC", "PV", label="names"),
            size = 2,
            box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"),
            color="black"
        )
    }

    if(!is.null(tPV))  {
        if(sum(dta$PV >= tPV) > 0) {
            plt <- plt + ggplot2::geom_hline(yintercept=tPV,
                linetype="dotdash", color="gray69", size=0.75)
        }
    }

    if(!is.null(tFC)) {
        if(sum(dta$FC <= -tFC) > 0) {
            plt <- plt + ggplot2::geom_vline(xintercept=-tFC,
                linetype="dotdash", color="gray69", size=0.75)
        }
    }

    if(!is.null(tFC)) {
        if(sum(dta$FC >= tFC) > 0) {
            plt <- plt + ggplot2::geom_vline(xintercept=tFC,
                linetype="dotdash", color="gray69", size=0.75)
        }
    }

    plt
}
