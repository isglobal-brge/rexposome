#' @describeIn ExposomePCA Plot correlation between exposures and PCA
#' @param exposure Vector of exposures to be shown in the plot
setMethod(
    f = "plotEXP",
    signature = "ExposomePCA",
    definition = function(object, exposure) {

        dta <- extract(object, table="correlation") #data.frame(object@pca$var$cor)

        if(missing(exposure)) {
            exposure <- rownames(dta)
        } else {
            if(sum(exposure %in% rownames(dta)) != length(exposure)) {
                stop("Invalid given value for 'exposure',")
            } else {
                dta <- dta[exposure, , drop = FALSE]
            }
        }

        dta$Exposure <- rownames(dta)
        xx.m <- reshape2::melt(dta)
        colnames(xx.m) <- c("Exposures", "Dim", "value")
        xx.m$Dim <- gsub("Dim.", "", xx.m$Dim)
        xx.m$Dim <- paste("PC", stringr::str_pad(xx.m$Dim, width = 2, pad = "0"))

        ggplot2::ggplot(xx.m, ggplot2::aes_string(x = "Dim", y = "Exposures")) +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(ggplot2::aes_string(fill = "value"), color = "white") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 90)
            ) +
            ggplot2::labs(fill="Correlation\n", colour="") +
            ggplot2::xlab("") + ggplot2::ylab("exposures") +
            ggplot2::scale_fill_gradient2(midpoint=0, low="red", mid="white",
                                          high="blue", space ="Lab")
    }
)
