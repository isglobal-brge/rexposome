#' @describeIn ExposomePCA Methdo to draw a 3D plot for PCA
#' @param cmpZ PC to place in Z-axis
#' @param main Title
#' @param angle Point of view
#' @param pch Size of the dots
#' @param legend Boolean to show or hide the legend
#' @param plines Boolean to show of hide the dotted lines that helps to place
#' the dots in the X/Y axes
setMethod(
    f = "plot3PCA",
    signature = "ExposomePCA",
    definition = function(object, cmpX, cmpY, cmpZ, phenotype, main, angle=35, pch=16, legend=TRUE, plines=TRUE) {
        if(!phenotype %in% colnames(pData(object@phenoData))) {
            stop("Given phenotype ('", phenotype, "') not in ExposomePCA")
        }

        dta <- extract(object, table="individuals")
        if(cmpX >= ncol(dta) | cmpY >= ncol(dta) | cmpZ >= ncol(dta)) {
            stop("Given value for 'cmpX', 'cmpY' or 'cmpZ' larger than computed ",
                 "components (ncp=", ncol(dta), ").")
        }
        dta$phenotype <- pData(object@phenoData)[, phenotype]

        clr <- grDevices::rainbow(length(unique(dta$phenotype)))
        names(clr) <- unique(dta$phenotype)

        xlab <- paste0("PC", stringr::str_pad(cmpX, 2, pad = 0))
        ylab <- paste0("PC", stringr::str_pad(cmpY, 2, pad = 0))
        zlab <- paste0("PC", stringr::str_pad(cmpZ, 2, pad = 0))

        if(missing(main)) {
            main <- paste0("3D space for PCA\n",
               xlab, " - ", ylab, " - ", zlab
            )
        }

        xlab <- paste0(xlab, " (", round(extract(object, table="eigen")[cmpX, 2], 2), "%)")
        ylab <- paste0(ylab, " (", round(extract(object, table="eigen")[cmpY, 2], 2), "%)")
        zlab <- paste0(zlab, " (", round(extract(object, tabke="eigen")[cmpZ, 2], 2), "%)")

        if(plines) {
            scatterplot3d::scatterplot3d(dta[ , cmpX], dta[ , cmpY], dta[ , cmpZ],
                 xlab=xlab,
                 ylab=ylab,
                 zlab=zlab,
                 box=TRUE,
                 pch=pch,
                 color=clr[dta$phenotype],
                 scale.y=.75,
                 angle=angle,
                 type="h",
                 lty.hplot=2,
                 main=main
            )
        } else {
            scatterplot3d::scatterplot3d(dta[ , cmpX], dta[ , cmpY], dta[ , cmpZ],
                 xlab=xlab,
                 ylab=ylab,
                 zlab=zlab,
                 box=TRUE,
                 pch=pch,
                 color=clr[dta$phenotype],
                 scale.y=.75,
                 angle=angle,
                 main=main
                 #type="h",
                 #lty.hplot=2
            )
        }

        if(legend) {
            legend("topright", inset=.05,
                bty="n", cex=.65,
                title=phenotype,
                names(clr), fill=clr
            )
        }
    }
)
