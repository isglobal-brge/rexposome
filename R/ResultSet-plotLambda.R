setMethod(
    f = "plotLambda",
    signature = "ResultSet",
    definition = function(object, trim=0.5, width=0.75) {
        tt <- data.frame(
            "exposure"=unique(rid(object)),
            "lambda"=sapply(rid(object), function(expo) {
                lambdaClayton(extract(object, rid=expo)$P.Value, trim=trim)
            })
        )

        ggplot2::ggplot(tt, ggplot2::aes(x=exposure, y=lambda, width=width)) +
            ggplot2::geom_bar(stat="identity") +
            ggplot2::ylab("lambda score") + ggplot2::xlab("") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
            )
    }
)
