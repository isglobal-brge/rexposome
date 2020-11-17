#' @describeIn ExposomeSet Draws an histogram of a given continuous exposure
#' or a pie chart if a given categorycal exposure.
#' @param density If set to \code{TRUE} a desntiry plot is draw on the
#' histogram
#' @param show.trans If set to \code{TRUE}, three extra plots are drawn
#' with usual transformations
setMethod(
    f = "plotHistogram",
    signature = "ExposomeSet",
    definition = function(x, select, density = TRUE, show.trans=FALSE) {

        if(sum(select %in% exposureNames(x)) != 1) {
            stop("Exposure '", select, "' not in ExposomeSet")
        }

        dta <- expos(x)[ , select, drop=FALSE]
        dta$rownames <- rownames(dta)
        dta <- reshape2::melt(dta, id.vars="rownames")
        colnames(dta) <- c("id", "exposure", "value")


        if(fData(x)[select, ".type"] == "numeric") {
            if(show.trans) {
                dta$exp <- exp(dta$value)
                dta$log <- log(dta$value)
                dta$sqrt <- sqrt(dta$value)

                pv <- data.frame(
                    raw = shapiro.test(dta$value)$p.value,
                    exp = shapiro.test(dta$exp)$p.value,
                    log = shapiro.test(dta$log)$p.value,
                    sqrt = shapiro.test(dta$sqrt)$p.value
                )

                pv <- vapply(pv, function(ii) {
                    paste0("(pval: ", format(ii, scientific=TRUE, digits=5), ")");
                }, FUN.VALUE = character(1))

                dta.r <- reshape2::melt(dta, id.vars=c("exposure", "id"))
                levels(dta.r$variable) <- paste(c("raw", "exp", "log", "sqrt"), pv)

                plt <- ggplot2::ggplot(dta.r, ggplot2::aes_string(x = "value")) +
                    ggplot2::geom_histogram(ggplot2::aes(y = ..density.., fill = I("Gainsboro"))) +
                    ggplot2::facet_wrap(~variable, scales="free") +
                    ggplot2::theme_bw()

            } else {
                plt <- ggplot2::ggplot(dta, ggplot2::aes_string(x = "value")) +
                    ggplot2::geom_histogram(ggplot2::aes(y = ..density.., fill = I("Gainsboro"))) +
                    ggplot2::theme_bw()
            }

            if(density) {
                plt <- plt + ggridges::geom_density_ridges(scales = 1)
            }



            return(plt)
        } else { # factor
            dta$value <- factor(dta$value)
            plt <- ggplot2::ggplot(dta, ggplot2::aes_string(x = "exposure")) +
                ggplot2::geom_bar(ggplot2::aes_string(fill = "value"), width = 1) +
                ggplot2::coord_polar(theta = "y") + ggplot2::theme_void() +
                ggplot2::scale_fill_discrete("") + ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5))
            return(plt)
        }
    }
)
