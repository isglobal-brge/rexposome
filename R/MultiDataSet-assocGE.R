setMethod(
    f = "assocGE",
    signature = "MultiDataSet",
    definition = function(object, formula, select, set="exposures", ...,
                          sva=FALSE, vfilter=NULL, ncores=1, verbose=FALSE,
                          warnings=TRUE) {
        ## ----------------------------------------------------------------- ##
        ## CHEKS
        ## ----------------------------------------------------------------- ##
        set <- match.arg(set, choices=c("exposures", "phenotypes"))

        tomic <- c(grep("expression", names(object)), grep("rnaseq", names(object)))
        texp <- grep("exposures", names(object))
        tcls <- grep("cluster", names(object))
        tomic <- names(object)[tomic]

        if(length(texp) != 0 & length(tcls) != 0) {
            stop("Given 'MultiDataSet' contains both 'ExposomeSet' and 'ExposomeClust'.")
        }

        tann <- ifelse(length(texp) == 0, "cluster", "exposure")
        texp <- ifelse(length(texp) == 0, names(object)[tcls], names(object)[texp])
        rm(tcls)

        if(length(tomic) != 1) {
            stop("One of the tables must exists in 'MultiDataSet' as ",
                 "'expression' or 'rnaseq'")
        }
        if(length(texp) != 1) {
            stop("One of the tables must exists in 'MultiData'Set' as ",
                 "'exposures'")
        }
        if(verbose) {
            message("The following tables will be used in the association ",
                    "process: ",paste0("'", paste(c(tomic, texp), collapse="', '"), "'"))
        }

        if(warnings | verbose) {
            warning("Sets from 'MultiDataSet' will be reduced to common samples")
        }

        l1 <- sapply(sampleNames(object)[c(tomic, texp)], length)
        object <- MultiDataSet:::commonSamples(object)
        l2 <- sapply(sampleNames(object)[c(tomic, texp)], length)
        l3 <- mapply('-', l1, l2, SIMPLIFY = FALSE)

        if(verbose) {
            message(paste(unlist(l3), names(l3),
                          sep = " samples were reduced from ", collapse = ", "))
        }
        ## ----------------------------------------------------------------- ##

        ## ----------------------------------------------------------------- ##
        ## CONVERT FORMULA
        ## ----------------------------------------------------------------- ##
        formula <- as.character(as.formula(formula))
        #exp.dt <- as.data.frame(object[[texp]])
        es <- object[[texp]]
        exp.dt <- cbind(pData(es), expos(es))
        rm(es)

        if(tann == "cluster") {
            ## ------------------------------------------------------------- ##
            ## EXPOSOME CLUSTER ANALYSIS
            ## ------------------------------------------------------------- ##
            if(warnings | verbose) {
                warning("Given 'MultiDataSet' contains an 'ExposomeClust'. Association test will be performed on clustering result.")
            }
            if(!missing(select) & (warnings | verbose)) {
                warning("Given values in 'select' argument. They will be dropped.")
            }
            select <- "cluster"
        } else {
            ## ------------------------------------------------------------- ##
            ## EXPOSOME SET ANALYSIS
            ## ------------------------------------------------------------- ##
            if(missing(select)) {
                if(set == "exposures") {
                    warning("No given 'select'. association will be computed for all exposures")
                    select <- exposureNames(object[[texp]])
                } else { ## set == "phenotypes"
                    warning("No given 'select'. association will be computed for all phenotypes")
                    select <- phenotypeNames(object[[texp]])
                }
            }
        }
        ## ----------------------------------------------------------------- ##


        ## ----------------------------------------------------------------- ##
        ## EXTRACT SETS AND PERFORM ANALYSIS
        ## ----------------------------------------------------------------- ##
        results <- lapply(select, function(ex) {
            design <- as.formula(paste0("~", ex, "+", formula[2]))
            if(verbose) {
                message("Evaluating model '", as.character(design), "'.")
            }
            pheno <- .create_p(
                expo.dt = exp.dt,
                omic.p = pData(object[[tomic]]),
                select = all.vars(design)
            )

            if(class(pheno) == "character") {
                stop("Invalid value '", pheno, "' in 'exposures' or 'covariates'")
            }

            na.loc <- rowSums(apply(pheno, 2, is.na))
            na.loc <- which(na.loc != 0)
            if(length(na.loc) != 0) {
                if(warnings | verbose) {
                    warning("There are missing values. ", length(na.loc),
                        " samples will be removed.")
                }
                pheno <- pheno[-na.loc, , drop=FALSE]
            } else {
                if(verbose) {
                    message("Testing '", ex, "' (", design, ")")
                }
            }


            if(sum(!sapply(sapply(apply(pheno, 2, table), length), ">", 1)) != 0) {
                warning("When testing for '", ex, "', at last one covariate ",
                        "is constant")
                list(
                    N=NA,
                    sva.num=NA,
                    design=NA,
                    result=NA,
                    error=paste0("Covariate in model '", as.character(design) ,"' is constant")
                )
            } else {
                tryCatch({
                    # Design model
                    design.mm <- model.matrix(formula(design), data = pheno)
                    gexp <- object[[tomic]][ , rownames(pheno), drop=FALSE]

                    # If required, apply SVA
                    n.sv <- NA
                    if(sva) {
                        if (verbose | warnings){
                            if(is.null(vfilter)) {
                                message("Computing SVA. This step can be very time consuming.",
                                    "Try using argument 'vfilter'.")
                            } else {
                                message("Computing SVA. This step can be very time consuming.")
                            }
                        }

                        n.sv <- sva::num.sv(Biobase::exprs(gexp),
                                            design.mm, vfilter=vfilter)
                        if (n.sv > 0){
                            svobj <- sva::sva(Biobase::exprs(gexp), design.mm,
                                    #design.mm[ , -1, drop=FALSE],
                                    n.sv=n.sv, vfilter=vfilter)
                            design.mm <- cbind(design.mm, svobj$sv)
                        }
                        rm(svobj)
                        suppressMessages(gc())
                    }

                    # Fit the model
                    if (verbose){
                        message("Fitting the model.")
                    }

                    fit <- limma::lmFit(gexp, design.mm, ...)
                    fit <- limma::eBayes(fit)

                    list(
                        N=nrow(pheno),
                        sva.num=n.sv,
                        error=NA,
                        design=design,
                        result=fit
                    )
                }, error=function(e) {
                    message(e)
                    return(list(
                        N=NA,
                        sva.num=NA,
                        error=e,
                        design=NA,
                        result=NULL
                    ))
                })
            }
        })
        names(results) <- select

        class_origin = c(
            ifelse(tann == "cluster", "ExposomeClust", "ExposomeSet"),
            "ExpressionSet")
        new("ResultSet",
            fun_origin = "assocGE",
            class_origin=class_origin,
            names = c(texp, tomic),
            results = results,
            fData = fData(object)[c(texp, tomic)],
            sva=ifelse(sva, 1, 0)
        )
        ## ------------------------------------------------------------- ##
})
