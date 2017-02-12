#' Function to draw a QQ Plot from a vector of numbers
#'
#' @param  values Numeric vector of P.Values
#' @return An object obtained from \link{ggplot}.
#' @export
qq_plot <- function(values) {
    values <- as.numeric(values)
    o <- -log10(sort(values, decreasing=F))
    e <- -log10(1:length(o)/length(o))
    c975 <- rep(0,length(o))
    c025 <- rep(0,length(o))
    for(i in 1:length(o)){
        c975[i] <- qbeta(0.975, i, length(o) - i + 1)
        c025[i] <- qbeta(0.025, i, length(o) - i + 1)
    }

    dta <- as.data.frame(cbind(e, o, c025, c975))

    p <- ggplot2::ggplot(dta)# + ggplot2::theme_bw()
    p <- p + ggplot2::geom_polygon(
            data=data.frame(
                e=c(dta$e, dta$e[length(dta$e):1]),
                o=c(-log10(dta$c025), -log10(dta$c975[length(dta$e):1]))
            ), ggplot2::aes(x=e, y=o), alpha=0.3
    )
    p <- p + ggplot2::geom_point(ggplot2::aes(x=e,y=o, colour=o, fill=o), alpha=0.5)
    p <- p + ggplot2::xlab(expression(Expected~~-log[10](italic( P-Value ))))
    p <- p + ggplot2::ylab(expression(Observed~~-log[10](italic( P-Value ))))
    p <- p + ggplot2::theme(legend.position = "none")
    return(p)
}
