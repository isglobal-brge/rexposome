#' @describeIn ExposomePCA Methdo to draw a 2D plot for PCA
#' @param object An \code{\link{ExposomePCA}} object
#' @param set Can take values \code{"exposures"}, \code{"samples"} or
#' \code{"all"}
#' @param cmpX PC to place in X-axis
#' @param cmpY PC to place in Y-axis
#' @param show.exposures When set to \code{TRUE}, labels for exposures are shown
#' @param show.samples When set to \code{TRUE}, labels for samples are shown
#' @param phenotype (when \code{set="samples"}) A phenotype can be selected
#' so the samples are coloured by its value.
setMethod(
    f = "plotPCA",
    signature = "ExposomePCA",
    definition = function(object, set, cmpX = 1, cmpY = 2, show.exposures=FALSE, show.samples=FALSE, phenotype) {

        if (set == "exposures") {
            .plot_exposures(object, cmpX, cmpY, show.exposures)
        } else if (set == "samples") {
            if(missing(phenotype)) {
                phenotype <- NA
            }
            .plot_phenotype(object, cmpX, cmpY, show.samples, phenotype)
        } else if (set == "all") {
            plt1 <- .plot_exposures(object, cmpX, cmpY, show.exposures) +
                ggplot2::theme(legend.position = "none") +
                ggplot2::ggtitle("Exposures Space")
            plt2 <- .plot_phenotype(object, cmpX, cmpY, show.samples, NA) +
                ggplot2::ggtitle("Samples Space")
            plt3 <- .plot_explained(object, cmpX, cmpY) +
                ggplot2::ggtitle("Explained Variance")
            plt4 <- .plot_acum(object, cmpX, cmpY) +
                ggplot2::ggtitle("Accum. Explained Variance")
            gridExtra::grid.arrange(plt1, plt2, plt3, plt4, ncol = 2)
        } else {
            stop("Invalid selected set.")
        }

    }
)



.plot_exposures <- function(object, cmpX, cmpY, show.exposures) {
    dta <- extract(object, table = "exposures") #data.frame(object@pca$var$coord)
    dta$Family <- pData(object@featureData)[rownames(dta), 1]
    dta$Label <- rownames(dta)

    if(cmpX >= ncol(dta) | cmpY >= ncol(dta)) {
        stop("Given value for 'cmpX' or 'cmpY' larger than computed ",
            "components (ncp=", ncol(dta)-1, ").")
    }

    plt <- ggplot2::ggplot(dta, ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank()
        ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "red", size = 1) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "red", size = 1) +
        ggplot2::geom_point(ggplot2::aes_string(color = "Family")) +
        ggplot2::xlab(paste0("PC", cmpX, " (", round(extract(object, table="eigen")[cmpX, 2], 2), "%)")) +
        ggplot2::ylab(paste0("PC", cmpY, " (", round(extract(object, table="eigen")[cmpY, 2], 2), "%)"))
    if(show.exposures) {
        ## Add labels for features
        plt <- plt + ggrepel::geom_text_repel(
            data = dta,
            ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY), label="Label"),
            size = 2,
            box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"),
            color="#222222",
            segment.color="#BBBBBB"
        ) + ggplot2::theme_bw() + ggplot2::theme(legend.position="none")
        ## /
    }
    plt
}

.plot_phenotype <- function(object, cmpX, cmpY, show.samples, phenotype) {
    dta <- data.frame(object@pca$ind$coord)
    if(!is.na(phenotype)){
        if(!phenotype %in% colnames(pData(object@phenoData))) {
            stop("Given phenotype ('", phenotype, "') not in ExposomePCA")
        }
        dta$phenotype <- factor(pData(object@phenoData)[, phenotype])
    }
    dta$Label <- rownames(dta)

    plt <- ggplot2::ggplot(dta, ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank()
        ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "red", size = 1) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "red", size = 1) +
        ggplot2::xlab(paste0("PC", cmpX, " (", round(extract(object, table="eigen")[cmpX, 2], 2), "%)")) +
        ggplot2::ylab(paste0("PC", cmpY, " (", round(extract(object, table="eigen")[cmpY, 2], 2), "%)"))

    if(is.na(phenotype)) {
        plt <- plt + ggplot2::geom_point()
    } else {
        plt <- plt + ggplot2::geom_point(ggplot2::aes_string(color = "phenotype"))
    }

    if(show.samples) {
        ## Add labels for features
        plt <- plt + ggrepel::geom_text_repel(
            data = dta,
            ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY), label="Label"),
            size = 2,
            box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"),
            color="#222222",
            segment.color="#BBBBBB"
        ) + ggplot2::theme_bw() + ggplot2::theme(legend.position="none")
        ## /
    }

    plt
}

.plot_explained <- function(object, cmpX, cmpY) {
    dta <- extract(object, table="eigen")[, 2, drop = FALSE]
    dta$componet <- paste("PC", stringr::str_pad(1:nrow(dta), pad = "0", width = 2), sep="")
    colnames(dta) <- c("exp", "cmp")
    dta$color <- "black"
    dta[c(cmpX, cmpY), "color"] <- "selected"
    dta$tt <- "A"
    dta$cmp <- factor(dta$cmp)

    rg <- 1:10
    if(nrow(dta) < 10) {
        rg <- 1:nrow(dta)
    }

    ggplot2::ggplot(dta[rg, ], ggplot2::aes_string("cmp", "exp", fill = "color")) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
        ggplot2::xlab("") + ggplot2::ylab("% variance explained") +
        ggplot2::scale_fill_manual(values = c("Gainsboro", "PaleGreen"),
            labels = c("black", "selected")) +
        ggplot2::geom_line(ggplot2::aes_string(group = "tt"), colour = "LightSeaGreen", size = 1.3) +
        ggplot2::geom_point(size = 2, colour = "LightSeaGreen")
}

.plot_acum <- function(object, cmpX, cmpY) {
    dta <- extract(object, table="eigen")[, 3, drop = FALSE]
    dta$componet <- paste("PC", stringr::str_pad(1:nrow(dta), pad = "0", width = 2), sep="")
    colnames(dta) <- c("acum", "cmp")
    dta$tt <- "A"
    dta$cmp <- factor(dta$cmp)

    rg <- 1:10
    if(nrow(dta) < 10) {
        rg <- 1:nrow(dta)
    }


    ggplot2::ggplot(dta[rg, ], ggplot2::aes_string("cmp", "acum")) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
        ggplot2::xlab("") + ggplot2::ylab("% variance explained") +
        ggplot2::scale_fill_manual(values = c("Gainsboro", "PaleGreen"),
            labels = c("black", "selected")) +
        ggplot2::geom_line(ggplot2::aes_string(group = "tt"), colour = "LightSeaGreen", size = 1.3) +
        ggplot2::geom_point(size = 2, colour = "LightSeaGreen") +
        ggplot2::geom_vline(xintercept = ifelse(cmpX >= cmpY, cmpX, cmpY),
            colour = "FireBrick", size = 1, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = ifelse(cmpX >= cmpY, object@pca$eig[cmpX,3],
                extract(object, table="eigen")[cmpY, 3]), colour = "FireBrick", size = 1,
            linetype = "dashed")
}
