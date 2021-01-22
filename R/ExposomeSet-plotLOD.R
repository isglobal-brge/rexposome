#' @describeIn ExposomeSet Draws a barchart with the amount of under-LOD values.
#' @param x.max Threshold for x axis (in \%)
#' @param sort If set to \code{TRUE}, results are ordered
setMethod(
    f = "plotLOD",
    signature = "ExposomeSet",
    definition = function(object, lod.col="LOD", x.max=100, sort=TRUE) {
        x <- tableLOD(object, output = "p", sort = sort)
        if(x.max > 100) x.max <- 100
        if(x.max < 0) x.max <- 1

        ggplot2::ggplot(data.frame(x),
                                ggplot2::aes(seq_along(x), x, fill = x)) +
            ggplot2::geom_bar(stat = "identity", width = 1) +
            ggplot2::coord_flip() +
            ggplot2::xlim(names(x)) +
            ggplot2::scale_fill_continuous(name = "%",
                breaks = seq(0, 100, 20),
                limits = c(0, 100), low="LightCoral", high="FireBrick") +
            ggplot2::ylab("% Values under LOD") +
            ggplot2::xlab("Exposures") +
            ggplot2::scale_y_continuous(limits = c(0, x.max))
    }
)
