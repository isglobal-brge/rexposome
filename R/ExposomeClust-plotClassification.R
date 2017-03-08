#' @describeIn ExposomeClust Draws a heatmap for the samples' classification.
setMethod(
  f = "plotClassification",
  signature = "ExposomeClust",
  definition = function(x, type="heatmap", ...) {
    type <- match.arg(type, c("heatmap", "valuemap"))
    if(type == "heatmap") {
      .cluster_heatmap(x, ...)
    } else if(type == "valuemap") {
      .cluster_valuemap(x, ...)
    } else {
      stop("Invalid type of plot.")
    }
  }
)

.cluster_valuemap <- function(x, family, scatter = TRUE) {
  # If family is 'all' all the exposome is shown
  if(missing(family)) {
    stop("'valuemap' for 'ExposomeClust' request argument 'family'.")
  }
  if(tolower(family) == "all") {
    return(.plot_exposome(x))
  }
  # /

  if (!family %in% familiesNames(x)) {
    stop("Given family '", family, "' not in ExposomeClust.")
  }
#   if (!is.na(group)) {
#     if (!group %in% phenotypesNames(x)) {
#       stop("Given group '", group, "' not in ExposomeClust.")
#     }
#   }

  ###.plot_cluster_numeric(x, family, ...)
#   typ <- .family_type(x, family)
#   if (typ == "numeric") {
#     .plot_cluster_numeric(x, family, ...)
#   } else if (typ == "factor") {
#     .plot_cluster_factor(x, family, ...)
#   } else {
#     stop("Plot for mixed family is not implemented.")
#   }
#}

###.plot_cluster_numeric <- function(x, family, scatter = TRUE) {
  data <- .get_exposures(x, family, group = "cluster")
  data$group <- paste("Group", data$group)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, color = group))
  if(scatter) {
    plot <- plot + ggplot2::geom_point(position = ggplot2::position_jitter(width=0.3), alpha=0.1)
    plot <- plot + ggplot2::geom_boxplot(alpha=0.1) + ggplot2::facet_wrap(~exposures)
  } else {
    plot <- plot + ggplot2::geom_boxplot() + ggplot2::facet_wrap(~exposures)
  }

  plot <- plot + ggplot2::scale_fill_brewer()
  plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  #plot <- plot + ggplot2::ylab("Measure")
  #plot <- plot + ggplot2::xlab("Exposure")
  plot <- plot + ggplot2::theme(legend.title = ggplot2::element_blank())
  plot
}

.cluster_heatmap <- function(x, cexRow = 0.5, cexCol = 1.1, adjCol = c(0.5, 1),
                             ...) {
  ff <- function(x, y) {
    aggregate(x, list(y), FUN=mean)$x
  }
  scaled <- scale(t(assayData(x)[["exp"]]))
  agrted <- t(apply(scaled, 2, ff, y=classification(x)))
  colnames(agrted) <- paste("Group", 1:ncol(agrted))

  gplots::heatmap.2(agrted, col = gplots::bluered(100), dendrogram = "row",
                    cexRow = cexRow, srtCol = 0, cexCol = cexCol, adjCol = adjCol,
                    density.info="none", tracecol = "darkgreen", lhei = c(2, 10), ...)

#   gplots::heatmap.2(agrted, col=gplots::redblue(100), key=FALSE, symkey=FALSE,
#                     trace="none", cexRow=0.5, density.info="none",
#                     dendrogram="row", srtCol=0, cexCol=1.1, adjCol = c(0.5,1),
#                     lmat=rbind(c(0, 3), c(2, 1), c(0, 4)), lhei=c(0.1, 6, .5), ...)
}
