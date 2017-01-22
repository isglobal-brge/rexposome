setMethod(
    f = "plotLambda",
    signature = "ResultSet",
    definition = function(object, width=0.75) {
        tt <- data.frame(
            "exposure"=unique(rid(object)),
            "lambda"=sapply(rid(object), function(expo) {
                qchisq(median(extract(object, rid=expo)$P.Value), df=2, lower.tail=FALSE)
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
