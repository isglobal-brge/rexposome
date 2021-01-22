#' @describeIn mExWAS Method to plot a heap-map with the coeficient of each
#' exposure
#' @param subtitles NOT USED
#' @param color NOT USED
#' @param exp.order NOT USED
#' @param show.effective NOT USED
#' @param labels NOT USED
#' @param ... Other used arguments.
setMethod(
    f = "plotExwas",
    signature = "mExWAS",
    definition = function(object, ...) {
        gTable <- function(it) {
            dta <- as.data.frame(as.matrix(stats::coef(it@result[[1]],
                c(it@result[[1]]$lambda.min, it@result[[1]]$lambda.1se))))
            dta <- cbind(dta[-1, ], rownames(it@description))
            dta$phe <- it@phenotype
            colnames(dta) <- c("Min", "1SE", "exposure", "phenotype")
            dta
        }

        multiple <- FALSE
        if(missing(...)) {
            items <- list(object)
        } else {
            multiple <- TRUE
            items <- mapply(c, list(object), list(...), SIMPLIFY=FALSE)[[1]]
        }

        dta <- do.call(rbind, lapply(items, gTable))
        rownames(dta) <- 1:nrow(dta)
        dta <- reshape2::melt(dta, id.vars=c("exposure", "phenotype"))

        if(!multiple) {
            plt <- ggplot2::ggplot(dta, ggplot2::aes_string(x = "variable", y = "exposure")) +
                ggplot2::geom_tile(ggplot2::aes_string(fill = "value"), color = "white") +
                ggplot2::theme_minimal() +
                ggplot2::labs(fill="Coefficients", colour="") +
                ggplot2::xlab("") + ggplot2::ylab("exposures") +
                ggplot2::scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                                          high="red", space ="Lab")
        } else {
            plt <- ggplot2::ggplot(dta, ggplot2::aes_string(x = "variable", y = "exposure")) +
                ggplot2::geom_tile(ggplot2::aes_string(fill = "value"), color = "white") +
                ggplot2::theme_minimal() +
                ggplot2::labs(fill="Coefficients", colour="") +
                ggplot2::xlab("") + ggplot2::ylab("exposures") +
                ggplot2::scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                                              high="red", space ="Lab") +
                ggplot2::facet_wrap(~phenotype) +
                ggplot2::theme(legend.position = "bottom")
        }
        return(plt)
    }
)
