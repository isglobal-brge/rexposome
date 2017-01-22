setMethod(
    f = "plotMissings",
    signature = "ExposomeSet",
    definition = function(object, set=c("exposures", "phenotypes"), x.max=100, sort=TRUE) {
        set <- match.arg(set, c("exposures", "phenotypes"))

        if(x.max > 100) x.max <- 100
        if(x.max < 0) x.max <- 1

        if(set == "exposures") {
            dta <- t(assayDataElement(object, "exp"))
        } else if(set == "phenotypes") {
            dta <- pData(object)
        }

        x <- apply(dta, MARGIN=2, function(colm) {
            sum(is.na(colm)) * 1.0 / length(colm)
        })

        if(sort) {
            x <- x[order(x, decreasing = TRUE)]
        }

        plot <- ggplot2::ggplot(data.frame(x) * 100,
            ggplot2::aes(seq_along(x), x, fill = x)) +
            ggplot2::geom_bar(stat = "identity", ggplot2::aes(width = 1))
        plot <- plot + ggplot2::xlim(names(x))
        plot <- plot + ggplot2::scale_fill_continuous(name = "%",
            breaks = seq(0, 100, 20),
            limits = c(0, 100), low="violet", high="violetred4")
        plot <- plot + ggplot2::ylab("% Missing Data")
        plot <- plot + ggplot2::xlab(capitalize(set))
        plot <- plot + ggplot2::coord_flip()
        plot <- plot + ggplot2::scale_y_continuous(limits = c(0, x.max))
        plot
  }
)
