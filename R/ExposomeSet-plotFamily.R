setMethod(
    f = "plotFamily",
    signature = "ExposomeSet",
    definition = function(x, family, group, group2, scatter = TRUE, na.omit=TRUE) {
        # If family is 'all' all the exposome is shown
        if(tolower(family) == "all") {
            return(.plot_exposome(x))
        }
        # /

        if (!family %in% familyNames(x)) {
            stop("Given family '", family, "' not in ExposomeSet (description).")
        }
        if (!missing(group)) {
            if (!group %in% phenotypeNames(x)) {
                stop("Given group '", group, "' not in ExposomeSet (phenotype).")
            }
        } else {
            group <- NA
        }
        if (!missing(group2)) {
            if (!group2 %in% phenotypeNames(x)) {
                stop("Given group (2)'", group2, "' not in ExposomeSet (phenotype).")
            }
            if(is.na(group)) {
                group <- group2
                group2 <- NA
            }
        } else {
            group2 <- NA
        }

        typ <- .family_type(x, family)
        if (typ == "numeric") {
            .plot_exposure_numeric(x, family, group, group2, scatter, na.omit)
        } else if (typ == "factor") {
            .plot_exposure_factor(x, family, group, group2, na.omit)
        } else {
            stop("Plot for mixed family is not implemented.")
        }

  }
)

.get_exposures <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- .family_type(x, family, as.type=TRUE)
    if (!is.na(group)) {
        if(!is.na(group2)) {
            gData <- pData(x)[, c(group, group2), drop=FALSE]
            colnames(gData) <- c("group1", "group2")
            n <- 2
        } else {
            gData <- pData(x)[, group, drop=FALSE]
            colnames(gData) <- "group"
            n <- 1
        }

        data <- cbind(data[rownames(gData), ], gData)
        if(!is.na(group2)) {
            data$group1 <- as.factor(data$group1)
            data$group2 <- as.factor(data$group2)
        } else {
            data$group <- as.factor(data$group)
        }

        data <- reshape2::melt(data, measure.vars = 1:(ncol(data)-n),
                           variable.factor = FALSE, rm.na = TRUE,
                           variable.name="exposures")

        if(na.omit) {
            if(!is.na(group2)) {
                data <- data[!is.na(data[, 1]), ]
                data <- data[!is.na(data[, 2]), ]
                data <- data[!is.na(data[, 4]), ]
            } else {
                data <- data[!is.na(data[, 1]), ]
                data <- data[!is.na(data[, 3]), ]
            }
        }

    } else {
        data <- reshape2::melt(data, measure.vars = 1:ncol(data),
                           variable.factor = FALSE, rm.na = TRUE,
                           variable.name="exposures")
        if(na.omit) {
            data <- data[!is.na(data[, 2]), ]
        }
    }
    return(data)
}

.plot_exposure_numeric <- function(x, family, group = NA, group2 = NA, scatter = TRUE, na.omit = TRUE) {
    data <- .get_exposures(x, family, group, group2, na.omit)

    # Design the plot
    if (!is.na(group)) {
        if(!is.na(group2)) {
            plot <- ggplot2::ggplot(data, ggplot2::aes(x = exposures, y = value, fill=group1))
            plot <- plot + ggplot2::geom_boxplot()
            plot <- plot + ggplot2::facet_wrap(~group2)
        } else {
            plot <- ggplot2::ggplot(data, ggplot2::aes(x = exposures, y = value, fill=group))
            plot <- plot + ggplot2::geom_boxplot()

        }
    } else {
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = exposures, y = value))
        if(scatter) {
            plot <- plot + ggplot2::geom_point(position = ggplot2::position_jitter(width=0.3), alpha=0.1)
            plot <- plot + ggplot2::geom_boxplot(alpha=0.1)
        } else {
            plot <- plot + ggplot2::geom_boxplot()
        }
    }
  plot <- plot + ggplot2::scale_fill_brewer()
  plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  plot <- plot + ggplot2::ylab("Measure")
  plot <- plot + ggplot2::xlab("Exposure")
  #plot <- plot + ggplot2::ggtitle(main)
  # /
  plot
}

.plot_exposure_factor <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- rexposome:::.get_exposures(x, family, group, group2, na.omit)

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = exposures, fill=value))
    if (!is.na(group)) {
        if(is.na(group2)) {
            plot <- plot + ggplot2::facet_wrap(~group)
        } else {
            plot <- plot + ggplot2::facet_wrap(group2~group1)
        }
    }

    plot <- plot + ggplot2::geom_bar(position = "fill")
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
    plot <- plot + ggplot2::ylab("Percent")
    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    plot <- plot + ggplot2::xlab("Exposure")

    # /
    plot
}



.plot_exposome <- function(obj) {
  .vplayout <- function(x, y) {
    grid::viewport(layout.pos.row = x, layout.pos.col = y)
  }

  nc <- round(sqrt(length(familyNames(obj))))
  if(nc * nc < length(familyNames(obj))) {
    nr <- nc + 1
  } else {
    nr <- nc
  }

  ff <- familyNames(obj)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nr, nc)))

  idx <- 1
  for(ii in 1:nr) {
    for(jj in 1:nc) {
      if(idx < length(ff) + 1) {
        plt <- plotFamily(obj, family = ff[idx], scatter = FALSE)
        plt <- plt + ggplot2::ggtitle(ff[idx])
        if(jj != 1) {
          plt <- plt + ggplot2::ylab("")
        }
        if(ii != nr) {
          plt <- plt + ggplot2::xlab("")
        }
        print(plt, vp = .vplayout(ii, jj))
        idx <- idx + 1
      }
    }
  }
  return(invisible())
}
