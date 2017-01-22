setMethod(
    f = "plot3PCA",
    signature = "ExposomePCA",
    definition = function(object, cmpX, cmpY, cmpZ, phenotype, main, angle=35, pch=16, legend=TRUE, plines=TRUE) {
        if(!phenotype %in% colnames(pData(object@phenoData))) {
            stop("Given phenotype ('", phenotype, "') not in ExposomePCA")
        }

        dta <- data.frame(object@pca$ind$coord)
        if(cmpX >= ncol(dta) | cmpY >= ncol(dta) | cmpZ >= ncol(dta)) {
            stop("Given value for 'cmpX', 'cmpY' or 'cmpZ' larger than computed ",
                 "components (ncp=", ncol(dta), ").")
        }
        dta$phenotype <- pData(object@phenoData)[, phenotype]

        clr <- rainbow(length(unique(dta$phenotype)))
        names(clr) <- unique(dta$phenotype)

        xlab <- paste0("PC", stringr::str_pad(cmpX, 2, pad = 0))
        ylab <- paste0("PC", stringr::str_pad(cmpY, 2, pad = 0))
        zlab <- paste0("PC", stringr::str_pad(cmpX, 2, pad = 0))

        if(missing(main)) {
            main <- paste0("3D space for PCA\n",
               xlab, " - ", ylab, " - ", zlab
            )
        }

        xlab <- paste0(xlab, " (", round(object@pca$eig[cmpX, 2], 2), "%)")
        ylab <- paste0(ylab, " (", round(object@pca$eig[cmpY, 2], 2), "%)")
        zlab <- paste0(zlab, " (", round(object@pca$eig[cmpX, 2], 2), "%)")

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
