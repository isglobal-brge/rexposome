setMethod(
    f = "correlation",
    signature = "ExposomeSet",
    definition = function(object, ..., warnings = TRUE) {
        cor.arg <- list()
        #crm.arg <- list()
        chis.arg <- list()
        lm.arg <- list()
        drop <- c("x", "y", "method")
        dots <- pryr::named_dots(...)
        for(name in names(dots)) {
            if(name %in% drop) {
                stop("Invalid argument '", name, "'. No arguments 'x', 'y' or ",
                    "'method' allowed. To add 'method' for 'lm' use 'method.lm'. ",
                    "To add 'method' for 'cor' use 'method.cor'.")
            }
            if(name %in% c(formalArgs(lm), "method.lm")) {
                if(name == "method.lm") { name <- "method" }
                    lm.arg[[name]] <- dots[[name]]
            } else if(name %in% c(formalArgs(cor), "method.cor")) {
                if(name == "method.cor") { name <- "method" }
                    cor.arg[[name]] <- dots[[name]]
            } else if(name %in% formalArgs(chisq.test)) {
                chis.arg[[name]] <- dots[[name]]
            # } else if(name %in% formalArgs(cramersV)) {
            #     crm.arg[[name]] <- dots[[name]]
            } else {
                stop("Argument '", name, "' do not corresponds to any argument ",
                "of 'cor', 'cramersV' nor 'lm' functions.")
            }
        }

        mtrc <- data.frame(t(assayDataElement(object, "exp")))
        #sel <- c()
        for(ex in colnames(mtrc)) {
            if(fData(object)[ex, "_type"] == "factor") {
                # if(length(unique(mtrc[ , ex])) > 2) {
                #     if(warnings) {
                #       warning("Exposure '", ex, "' will be droped to be multicategorical.")
                #     }
                # } else {
                mtrc[ , ex] <- factor(mtrc[ , ex])
                #   sel <- c(sel, ex)
                # }
            } else {
                mtrc[ , ex] <- as.numeric(mtrc[ , ex])
                # sel <- c(sel, ex)
            }
        }

        # cr <- .corr_exposures(mtrc[ , sel], object[sel, ], cor.arg, crm.arg, lm.arg)
        cr <- .corr_exposures(mtrc, object, cor.arg, chis.arg, lm.arg, warnings)

        new("ExposomeCorr",
            assayData = assayDataNew("environment", corr = t(cr)),
            #featureData = featureData(object[sel, ])
            featureData = featureData(object)
        )
    }
)



.corr_exposures <- function(mtrc, object, cor.arg, chis.arg, lm.arg, warnings) {
    # lm.beta <- function (x) {
    #     b <- coef(x)[-1]
    #     sx <- sd(x$model[,-1])
    #     sy <- sd(x$model[,1])
    #     beta <- b * sx/sy
    #     return(beta)
    # }
    mtrc <<- mtrc
    xx <- do.call(rbind, lapply(colnames(mtrc), function(ex_i) {
        ty_i <- fData(object)[ex_i, "_type"]
        yy <- sapply(colnames(mtrc), function(ex_j) {
            ty_j <- fData(object)[ex_j, "_type"]

            if(ty_i == "numeric" & ty_j == "numeric") {
                # Both exposures are numeric
                #   We can use the standard cor function for base
                do.call("cor", c(list(x = mtrc[ , c(ex_i, ex_j)]), cor.arg))[1, 2]
            } else if(ty_i == "factor" & ty_j == "factor") {
                # Both exposures are factor
                #   If both exposures are dictomical we could use
                #   5th caramer factor as estimator for correlation. When
                #   the exposures are multicategorical we need to use
                #   the pval from a Chi Square test as etimator of the correlation.
                # do.call("cramersV", c(list(x = table(mtrc[ , c(ex_i, ex_j)])), crm.arg))
                tryCatch({
                    xx <- do.call("chisq.test", c(list(x = table(mtrc[ , c(ex_i, ex_j)])), chis.arg))
                    xx$p.value
                }, error=function(e) {
                    if(warnings) {
                        warning(ex_i, " - ", ex_j, ": ", e)
                    }
                    NA
                })

            } else { # if(ty_i == "numeric" & ty_j == "factor") {
                # One of the exposures is numeric and the other is facor
                #   If the factor exposures is multicategorical we can use the
                #   r from multiple regresion model as estimator for the
                #   correlation. An alternative could be to look for the eta of
                #   a one-way ANOVA.
                tryCatch({
                    fm <- paste(ex_i, "~", ex_j)
                    if(ty_i == "factor") {
                        fm <- paste(ex_j, "~", ex_i)
                    }
                    fm <- do.call("lm", c(list(formula = fm,
                        data = mtrc[ , c(ex_i, ex_j)]), lm.arg))
                    # lm.beta(fm)
                    sqrt(summary(fm)$r.squared)
                }, error = function(e) {
                    if(warnings) {
                        warning(ex_i, " - ", ex_j, ": ", e)
                    }
                    NA
                })
            }
        })
        names(yy) <- colnames(mtrc)
        yy
    }))
    rownames(xx) <- colnames(mtrc)
    xx
}




