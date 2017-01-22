setMethod(
    f = "mexwas",
    signature = "ExposomeSet",
    definition = function(object, phenotype, family, warnings = TRUE) {
        if(sum(phenotype %in% colnames(pData(object))) != 1) {
            stop("Given phenotype (", phenotype, ") not in ExposomeSet.")
        }

        dta <- as.data.frame(object, phe=FALSE)
        if(sum(is.na(dta)) != 0) {
            stop("Exposures data has 'NA' values.")
        }

        phe <- pData(object)[ , phenotype, drop=FALSE]
        if(sum(is.na(phe)) != 0) {
            warning("Given phenotype (", phenotype, ") has NA values. ",
                    sum(is.na(phe)), " samples will be discarded.")
            phe <- phe[!is.na(phe), , drop=FALSE]
        }
        dta <- dta[rownames(phe), ]
        phe <- phe[ , 1]

        ## --------------------------------------------------------------------

        x <- as.matrix(dta)
        x <- apply(x, 2, as.numeric)
        if(family %in% c("binomial", "multinomial")) phe <- as.factor(phe)
        cvfit <- glmnet::cv.glmnet(x, phe, family = family,type.measure="auc")
        fit <- glmnet::glmnet(x, phe, family = family)

        new("mExWAS",
            result = list(cvfit, fit),
            description = fData(object),
            phenotype = phenotype
        )
    }
)


function(object, verbose = FALSE, warnings = TRUE) {
    object <- expo

    if(verbose) {
        message("Computing correlation between exposures.")
    }
    cr <- extract(correlation(object, use = "pairwise.complete.obs", method.cor = "pearson"))
    cr <- abs(cr)

    ## Select exposures with a correlation over 0.9
    re <- list()
    kk <- 1
    for(ii in 1:(nrow(cr) - 1)) {
        for(jj in (ii+1):ncol(cr)) {
            if(cr[ii, jj] > 0.9) {
                re[[kk]] <- c(rownames(cr)[ii], rownames(cr)[jj])
                kk <- kk + 1
            }
        }
    }
    rm(kk)
    re <- unique(unlist(re))

    if(warnings & length(re) != 0) {
        warning("There are ", length(re), " exposures with correlations over 0.9. Those exposures will be excluded from the analysis.")
    }

    sel <- colnames(cr)[!colnames(cr) %in% re]
    rm(re)
    ## /

    ## Get exposures and remove those with high correlation
    dta <- as.data.frame(object, phe = FALSE)
    message(ncol(dta))
    dta <- dta[ , sel]
    message(ncol(dta))
    ## /

    ## Add phenotype and perfom DSA
    dta <- cbind(dta, pData(object)[ , phenotype, drop=FALSE])
    mod <- DSA::DSA(formula(paste0(phenotype, " ~ 1")),
               data = dta,
               maxsize = ncol(dta),
               maxorderint = 1,
               maxsumofpow = 1,
               id = rownames(dta)
    )
    ## /

    summary(mod)

}
