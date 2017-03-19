#' @describeIn imExposomeSet Draws a boxplot or accumulated-bar plot for each
#' exposure in the all imputed sets.
#' @param group Phenotype to group exposures
#' @param group2 NOT USED
#' @param scatter If set to true it shows the samples value in the plot
#' @param na.omit NOT USED
setMethod(
    f = "plotFamily",
    signature = "imExposomeSet",
    definition = function(x, family, group, group2, scatter = FALSE, na.omit=TRUE) {
        if(missing(family)) {
            stop("Required 'family' argument.")
        }
        if(!family %in% unique(x@featureData$Family)) {
            stop("Given family '", family, "' not in the imExposomeSet.")
        }
        exposures <- as.character(x@featureData$Exposure[x@featureData$Family == family])
        type <- unique(x@featureData$`_type`[x@featureData$Family == family])
        exposures <- x@assayData[ , c(".imp", exposures), drop=FALSE]
        exposures <- reshape2::melt(exposures, id.var=".imp")
        exposures[ , ".imp"] <- paste0("Imp ", exposures[ , ".imp"])

        if(type == "factor") {
            plot <- ggplot2::ggplot(exposures, ggplot2::aes_string(x = ".imp", fill = "value"))
            plot <- plot + ggplot2::facet_wrap(~variable)
            plot <- plot + ggplot2::geom_bar(position = "fill")
            plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
            plot <- plot + ggplot2::ylab("Percent")
            plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
            plot <- plot + ggplot2::xlab("Imputation") +
                ggplot2::theme(legend.position = "bottom")
        } else if(type == "numeric") {
            plot <- ggplot2::ggplot(exposures, ggplot2::aes_string(x = ".imp", y = "value"))
            plot <- plot + ggplot2::facet_wrap(~variable)
            if(scatter) {
                plot <- plot + ggplot2::geom_point(position = ggplot2::position_jitter(width=0.3), alpha=0.1)
                plot <- plot + ggplot2::geom_boxplot(alpha=0.1)
            } else {
                plot <- plot + ggplot2::geom_boxplot()
            }
            plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
            plot <- plot + ggplot2::xlab("Imputation")
        }

        return(plot)
    }
)
