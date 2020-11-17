#' @describeIn ExWAS Draws a plot with the confidence interval of each
#' exposure. Alows to compare two ExWAS instances.
# @param x An \code{\link{ExWAS}} object
#' @param y An \code{\link{ExWAS}} object
#' @param select (optional) Vector with the selected exposures
#' @param xlab (optional) Label for x-axis
#' @param ylab (optional) Label for y-axis
setMethod(
    f = "plotEffect",
    signature = "ExWAS",
    definition = function(x, y, select, labels, xlab, ylab) {
        xr <- x
        x <- extract(x)
        x$exposure <- sapply(strsplit(rownames(x), "\\$"), function( it ) {
            ifelse(length( it ) == 1, it, paste0(it[1], " (", it[2], ")"))
        })
        x$lbl <- rownames(x)
        if(!missing(labels)) {
            x$lbl <- sapply(strsplit(rownames(x), "\\$" ), function(nm) {
                ex <- ifelse(nm[1] %in% names(labels), labels[nm[1]], nm[1])
                if(length(nm) == 2) {
                    ex <- paste0(ex, " (", nm[2], ")")
                }
                ex
            })
        }

        if(!missing(y)) {
            y$lbl <- rownames(y)
            if(!missing(labels)) {
                y$lbl <- sapply(strsplit(rownames(y), "\\$" ), function(nm) {
                    ex <- ifelse(nm[1] %in% names(labels), labels[nm[1]], nm[1])
                    if(length(nm) == 2) {
                        ex <- paste0(ex, " (", nm[2], ")")
                    }
                    ex
                })
            }
        }

        if(missing(y)) {
            colnames(x)[3:4] <- c("minE", "maxE")
            if(missing(select)) {
                select <- rownames(x)
            } else {
                if(sum(select %in% x$exposure) != length(select)) {
                    stop("Selected exposures are not in given ExWAS")
                }
            }
            ggplot2::ggplot(as.data.frame(x[select, ]), ggplot2::aes_string(x = "effect", y = "lbl")) +
                ggplot2::geom_point(shape=18, size=5, color="gray60") +
                ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "minE", xmax = "maxE")) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "WhiteSmoke", size = 0.3, linetype = "dashed"),
                    panel.grid.minor = ggplot2::element_line(color = "gray40", size = 0.3, linetype = "dashed")
                ) + ggplot2::ylab("")
        } else {

            yr <- y
            y <- extract(y)
            y$exposure <- sapply(strsplit(rownames(y), "\\$"), function( it ) {
                ifelse(length( it ) == 1, it, paste0(it[1], " (", it[2], ")"))
            })
            exposures <- intersect(x$exposure, y$exposure)

            if(length(exposures) == 0) {
                stop("Given ExWAS has no common exposues.")
            }

            if(length(exposures) != nrow(x) | length(exposures) != nrow(y)) {
                warning("Given ExWAS have different exposures. Only common exposures will be used.")
            }

            z <- merge(x[exposures, ], y[exposures, ], by="exposure")
            colnames(z)[c(4:5, 8:9)] <- c("minE.x", "maxE.x", "minE.y", "maxE.y")

            if(missing(select)) {
                select <- z$exposure
            } else {
                if(sum(select %in% z$exposure) != length(select)) {
                    stop("Selected exposures are not in combined ExWAS")
                }
            }

            rownames(z) <- z$exposure
            if(missing(xlab)) {
                xlab <- as.character(xr@formula)
                xlab <- paste(xlab[2], "~", paste(xlab[3:length(xlab)], collapse=" + "), collapse=" ")
            }
            if(missing(ylab)) {
                ylab <- as.character(yr@formula)
                ylab <- paste(ylab[2], "~", paste(ylab[3:length(ylab)], collapse=" + "), collapse=" ")
            }

            ggplot2::ggplot(as.data.frame(z[select, ]), ggplot2::aes_string(x = "effect.x", y = "effect.y")) +
                ggplot2::geom_point(shape=18, size=5, color="gray60") +
                ggplot2::theme_bw(base_size = 17) +
                ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "minE.y", ymax = "maxE.y")) +
                ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "minE.x", xmax = "maxE.x")) +
                ggplot2::geom_abline(colour = "blue") +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "gray20", size = 0.3, linetype = "dashed"),
                    panel.grid.minor = ggplot2::element_line(color = "gray40", size = 0.3, linetype = "dashed")
                ) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
        }

    }
)
