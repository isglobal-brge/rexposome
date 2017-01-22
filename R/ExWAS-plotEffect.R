setMethod(
    f = "plotEffect",
    signature = "ExWAS",
    definition = function(x, y, select, x_lab, y_lab) {
        xr <- x
        x <- extract(x)
        x$exposure <- rownames(x)
        if(missing(y)) {
            colnames(x)[3:4] <- c("minE", "maxE")
            if(missing(select)) {
                select <- x$exposure
            } else {
                if(sum(select %in% x$exposure) != length(select)) {
                    stop("Selected exposures are not in given ExWAS")
                }
            }
            ggplot2::ggplot(x[select, ], ggplot2::aes(x=effect, y=exposure)) +
                ggplot2::geom_point(shape=18, size=5, color="gray60") +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin=minE, xmax=maxE)) +
                ggplot2::theme_bw(base_size = 17) +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "WhiteSmoke", size = 0.3, linetype = "dashed"),
                    panel.grid.minor = ggplot2::element_line(color = "gray40", size = 0.3, linetype = "dashed")
                )
        } else {
            yr <- y
            y <- extract(y)
            y$exposure <- rownames(y)
            exposures <- intersect(x$exposure, y$exposure)

            if(length(exposures) == 0) {
                stop("Given ExWAS has no common exposues.")
            }

            if(length(exposures) != nrow(x) | length(exposures) != nrow(y)) {
                warning("Given ExWAS have different exposures. Only common exposures will be used.")
            }

            z <- merge(x[exposures, ], y[exposures, ], by="exposure")
            colnames(z)[c(4:5, 9:10)] <- c("minE.x", "maxE.x", "minE.y", "maxE.y")

            if(missing(select)) {
                select <- z$exposure
            } else {
                if(sum(select %in% z$exposure) != length(select)) {
                    stop("Selected exposures are not in combined ExWAS")
                }
            }

            rownames(z) <- z$exposure
            if(missing(x_lab)) {
                x_lab <- as.character(xr@formula)
                x_lab <- paste(x_lab[2], "~", paste(x_lab[3:length(x_lab)], collapse=" + "), collapse=" ")
            }
            if(missing(y_lab)) {
                y_lab <- as.character(yr@formula)
                y_lab <- paste(y_lab[2], "~", paste(y_lab[3:length(y_lab)], collapse=" + "), collapse=" ")
            }

            ggplot2::ggplot(z[select, ], ggplot2::aes(x=effect.x, y=effect.y)) +
                ggplot2::geom_point(shape=18, size=5, color="gray60") +
                ggplot2::theme_bw(base_size = 17) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin=minE.y, ymax=maxE.y)) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin=minE.x, xmax=maxE.x)) +
                ggplot2::geom_abline(colour = "blue") +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "gray20", size = 0.3, linetype = "dashed"),
                    panel.grid.minor = ggplot2::element_line(color = "gray40", size = 0.3, linetype = "dashed")
                ) + ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab)
        }

    }
)
