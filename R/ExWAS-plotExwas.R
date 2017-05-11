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
    definition = function(object, ..., subtitles, color, show.effective = TRUE) {


        multiple <- FALSE
        if(missing(...)) {
            items <- list(object)
        } else {
            multiple <- TRUE
            items <- c(list(object), list(...))
        }

        if(!missing(subtitles)) {
            if(length(items) == 1) {
                warning("Given 'subtitles' when single 'ExWAS' present.",
                    "'subtitles' will not be used.")
            } else if(length(subtitles) != length(items)) {
                stop("Diffrent lenghts beween given objects and 'subtitles'.")
            }
        }

        tbl <- data.frame(pvalue=0.0, effect=0.0, x2.5=0.0, x97.5=0.0,
                         fm="", lpv=0.01, exposures="", family="" )

        for(ii in length(items)) {
            it <- items[ii]
            tbli <- extract(it)
            colnames(tbli) <- c("pvalue", "effect", "x2.5", "x97.5")
            if(missing(subtitles)) {
                ff <- as.character(it@formula)
                ff <- paste0(ff[2], ff[1], gsub(" ", "", ff[3]))
                tbli$fm <- ff
            } else {
                tbli$fm <- subtitles[ii]
            }
            tbli$lpv <- -log10(tbli$pvalue)
            tbli$exposure <- rownames(tbli)
            tbli$family <- object@description[rownames(tbli), 1]
            tbl <- rbind(tbl, tbli)

        }
        tbl <- tbl[-1, ]
        rownames(tbl) <- 1:nrow(tbl)
        colnames(tbli)[1:4] <- c("pvalue", "effect", "2.5", "97.5")

        nm <- unique(as.character(tbl$family))
        if(missing(color)) {
            colorPlte <- sample(grDevices::rainbow(length(nm)))
            names(colorPlte) <- nm
        } else {
            colorPlte <- color
            names(colorPlte) <- names(color)
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
                    values = colorPlte) +
                ggplot2::theme(legend.position = "right")
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
