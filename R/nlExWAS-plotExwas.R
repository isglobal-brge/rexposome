#' @describeIn nlExWAS Method to raw a bar-plot with the AIC of each model
#' @aliases nlExWAS-methods
setMethod(
    f = "plotExwas",
    signature = "nlExWAS",
    definition = function(object) {
        tbl <- extract(object)
        colnames(tbl)[1] <- "exposure"
        tbl$color <- ifelse(tbl$AIC > 0, "over", "under")
        tbl$color[is.na(tbl$AIC)] <- NA

        ggplot2::ggplot(tbl, ggplot2::aes(x = exposure, y = AIC)) +
            ggplot2::geom_bar(stat="identity", ggplot2::aes(fill = color)) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = c("CornflowerBlue", "IndianRed"),
                                       labels = c("over", "under")) +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
)
