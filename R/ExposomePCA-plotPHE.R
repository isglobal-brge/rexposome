#' @describeIn ExposomePCA Plot association score between phentoypes and PCA
#' @param exp2fac Number of different values to considere an exposures
#' continuous
setMethod(
    f = "plotPHE",
    signature = "ExposomePCA",
    definition = function(object, phenotype, exp2fac = 5) {
        dta <- data.frame(object@pca$ind$coord)
        phe <- pData(object@phenoData)

        if(missing(phenotype)) {
            phenotype <- colnames(phe)
        }

        dim <- colnames(dta)
        dta <- cbind(dta, phe)

        xx <- data.frame(t(do.call(rbind, lapply(phenotype, function(ph) {
            typ <- rexposome:::.pheno_type(object, ph, exp2fac)
            dta <- dta[!is.na(dta[ , ph]), ]
            vapply(dim, function(nc) {
                if(typ == "factor") {
                    dta[ , ph] <- as.factor(dta[ , ph])
                    summary(stats::glm(as.formula(paste0(ph, "~", nc)),
                        data = dta, family = "binomial"))$coefficients[2, 4]
                } else if(typ == "numeric") {
                    dta[ , ph] <- as.numeric(dta[ , ph])
                    summary(stats::glm(as.formula(paste0(ph, "~", nc)),
                        data = dta, family = "gaussian"))$coefficients[2, 4]
                }
            }, FUN.VALUE = numeric(1))
        }))))
        colnames(xx) <- phenotype
        xx$Dim <- rownames(xx)

        xx.m <- reshape2::melt(xx)
        xx.m$Dim <- gsub("Dim.", "", xx.m$Dim)
        xx.m$Dim <- paste("PC", stringr::str_pad(xx.m$Dim, width = 2, pad = "0"))

        xx.m$value2 <- ifelse(xx.m$value < 0.001, "A) <0.001",
            ifelse(xx.m$value >= 0.001 & xx.m$value < 0.005, "B) 0.001-0.005",
            ifelse(xx.m$value >= 0.005 & xx.m$value < 0.01,  "C) 0.005-0.01",
            ifelse(xx.m$value >= 0.01  & xx.m$value < 0-05,  "D) 0.01-0.05", "E) >0.05"
        ))))

        cbPalette <- c("#009E73",  "#56B4E9", "#E69F00", "#999999")#, "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

        xx.m$PV <- -log10(xx.m$value)
        ggplot2::ggplot(xx.m, ggplot2::aes_string(x = "Dim", y = "variable")) +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(ggplot2::aes_string(fill = "PV"), color = "white") +
            ggplot2::theme(
                 axis.text.x = ggplot2::element_text(angle = 90)
            ) +
            ggplot2::labs(fill="-log10(P-Value)\n", colour="") +
            ggplot2::xlab("") + ggplot2::ylab("phenotype") +
            ggplot2::scale_fill_continuous(limits = c(0, max(-log10(xx.m$value))), low="White", high="Navy")
    }
)
