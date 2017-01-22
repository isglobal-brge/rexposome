setMethod(
    f = "plotExwas",
    signature = "mExWAS",
    definition = function(object) {

        dta <- as.data.frame(as.matrix(stats::coef(object@result[[1]], c(object@result[[1]]$lambda.min, object@result[[1]]$lambda.1se))))
        dta <- cbind(dta[-1, ], rownames(object@description))

        colnames(dta) <- c("Min", "1SE", "exposure")
        dta <- reshape2::melt(dta, id.vars="exposure")

        ggplot2::ggplot(dta, ggplot2::aes(x = variable, y = exposure)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), color = "white") +
            ggplot2::theme_minimal() +
            ggplot2::labs(fill="Coefficients", colour="") +
            ggplot2::xlab("") + ggplot2::ylab("exposures") +
            ggplot2::scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                                          high="red", space ="Lab")
    }
)
