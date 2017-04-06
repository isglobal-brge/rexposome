#' @describeIn ExWAS Method to plot a manhatan plot for association between
#' exposures and phenitypes
#' @param object An object of class \code{ExWAS}, \code{mExWAS} or \code{nlExWAS}.
#' @param color (optional) A vector of colors. The vector must have length
#' equal to the number of families. The vector must be names with the
#' name of the families.
#' @param show.effective (default TRUE) draws a brown line on the
#' threshold given by the effective number of tests.
setMethod(
    f = "plotExwas",
    signature = "ExWAS",
    definition = function(object, ..., color, show.effective = TRUE) {

        multiple <- FALSE
        if(missing(...)) {
            items <- list(object)
        } else {
            multiple <- TRUE
            items <- mapply(c, list(object), list(...), SIMPLIFY=FALSE)[[1]]
        }
        tbl <- do.call(rbind, lapply(items, function(it) {
            tbl <- extract(it)
            ff <- as.character(it@formula)
            ff <- paste0(ff[2], ff[1], gsub(" ", "", ff[3]))
            tbl$fm <- ff
            tbl$lpv <- -log10(tbl$pvalue)
            tbl$exposure <- rownames(tbl)
            tbl$family <- object@description[rownames(tbl), 1]
            tbl
        }))
        rownames(tbl) <- 1:nrow(tbl)

        nm <- unique(as.character(tbl$family))
        if(missing(color)) {
            colorPlte <- sample(grDevices::rainbow(length(nm)))
            names(colorPlte) <- nm
        } else {
            colorPlte <- color
        }

        tbl <- tbl[order(tbl$family, tbl$exposure), ]
        tbl$exposure <- factor(tbl$exposure, levels = unique(tbl$exposure))

        if(!multiple) {
            plt <- ggplot2::ggplot(tbl, ggplot2::aes_string(x = "lpv", y = "exposure", color = "family")) +
                ggplot2::geom_point() +
                ggplot2::theme_minimal() +
                ggplot2::theme(panel.spacing = ggplot2::unit(0.5, 'lines'),
                    strip.text.y = ggplot2::element_text(angle = 0)) +
                ggplot2::ylab("") +
                ggplot2::xlab(expression(-log10(pvalue))) +
                ggplot2::labs(colour="Exposure's Families") +
                ggplot2::scale_color_manual(breaks = names(colorPlte),
                    values = colorPlte)
        } else {
            plt <- ggplot2::ggplot(tbl, ggplot2::aes_string(x = "lpv", y = "exposure", color = "family")) +
                ggplot2::geom_point() +
                ggplot2::theme_minimal() +
                ggplot2::theme(panel.spacing = ggplot2::unit(0.5, 'lines'),
                    strip.text.y = ggplot2::element_text(angle = 0)) +
                ggplot2::ylab("") +
                ggplot2::xlab(expression(-log10(pvalue))) +
                ggplot2::labs(colour="Exposure's Families") +
                ggplot2::scale_color_manual(breaks = names(colorPlte),
                    values = colorPlte) +
                ggplot2::facet_wrap(~fm) +
                ggplot2::theme(legend.position = "bottom")
        }
        if(show.effective) {
            plt <- plt + ggplot2::geom_vline(xintercept = -log10(object@effective), colour="Brown")
        }

        return(plt)
})
