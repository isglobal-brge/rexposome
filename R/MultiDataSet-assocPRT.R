setMethod(
    f = "assocPRT",
    signature = "MultiDataSet",
    definition = function(object, formula, select, set="exposures", ...,
                          ebayes=FALSE, sva=FALSE, vfilter=NULL, ncores=1, verbose=FALSE,
                          warnings=TRUE) {
        ## ----------------------------------------------------------------- ##
        ## CHEKS
        ## ----------------------------------------------------------------- ##
        set <- match.arg(set, choices=c("exposures", "phenotypes"))

        tpro <- grep("protein", names(object))
        texp <- grep("exposures", names(object))
        tcls <- grep("cluster", names(object))
        tpro <- names(object)[tpro]

        if(length(texp) != 0 & length(tcls) != 0) {
            stop("Given 'MultiDataSet' contains both 'ExposomeSet' and 'ExposomeClust'.")
        }

        tann <- ifelse(length(texp) == 0, "cluster", "exposure")
        texp <- ifelse(length(texp) == 0, names(object)[tcls], names(object)[texp])
        rm(tcls)

        if(length(tpro) != 1) {
            stop("One of the tables must exists in 'MultiDataSet' as ",
                 "'protein'")
        }
        if(length(texp) != 1) {
            stop("One of the tables must exists in 'MultiData'Set' as ",
                 "'exposures' or 'cluster'.")
        }
        if(verbose) {
            message("The following tables will be used in the association ",
                    "process: ",paste0("'", paste(c(tpro, texp), collapse="', '"), "'"))
        }
        if(warnings | verbose) {
            warning("Sets from 'MultiDataSet' will be reduced to common samples")
        }

        l1 <- sapply(sampleNames(object)[c(tpro, texp)], length)
        object <- MultiDataSet:::commonSamples(object)
        l2 <- sapply(sampleNames(object)[c(tpro, texp)], length)
        l3 <- mapply('-', l1, l2, SIMPLIFY = FALSE)

        if(verbose) {
            message(paste(unlist(l3), names(l3),
                          sep = " samples were reduced from ", collapse = ", "))
        }
        ## ----------------------------------------------------------------- ##
        formula <- as.character(as.formula(formula))
        exp.dt <- as.data.frame(object[[texp]])

        if(tann == "cluster") {
            ## ------------------------------------------------------------- ##
            ## EXPOSOME CLUSTER ANALYSIS
            ## ------------------------------------------------------------- ##
            if(warnings | verbose) {
                warning("Given 'MultiDataSet' contains an 'ExposomeClust'. Association test will be performed on clustering result.")
            }
            design <- as.formula(paste0("~cluster+", formula[2]))
            pheno <- rexposome:::.create_p(
                expo.dt = exp.dt,
                omic.p = pData(object[[tpro]]),
                select = all.vars(design)
            )
            if(class(pheno) == "character") {
                stop("Invalid value '", pheno, "' in 'exposures' or 'covariates'")
            }
            if(sum(is.na(pheno)) != 0 & (warnings | verbose)) {
                warning("Given design ('", design, "') has ",
                        sum(is.na(pheno)), " NA values")
            }

            na.loc <- rowSums(apply(pheno, 2, is.na))
            na.loc <- which(na.loc != 0)
            if(length(na.loc) != 0){
                warning("There are missing values. ", length(na.loc),
                        " samples will be removed.")
                pheno <- pheno[-na.loc, , drop=FALSE]
            }

            # -----------------------------------------------------------------

            if(sum(!sapply(sapply(apply(pheno, 2, table), length), ">", 1))) {
                warning("When testing for '", ex, "', at last one covariate ",
                        "is constant")
                new("ResultSet",
                    fun_origin = "assocPRT",
                    class_origin = c("ExposomeClust", "ExpressionSet"),
                    names = c(texp, tpro),
                    results = list("cluster"=list(
                            N=NA,
                            design=NA,
                            result=NULL,
                            error="covariate is constant"
                        )),
                    fData = fData(object)[c(texp, tpro)],
                    sva=ifelse(sva, 1, 0)
                )

            } else {
                results <- tryCatch({
                    # Design model
                    design.mm <- model.matrix(formula(design), data = pheno)
                    prot <- object[[tpro]][ , rownames(pheno), drop=FALSE]

                    # If required, apply SVA
                    if(sva) {
                        if (verbose | warnings){
                            message("Computing SVA. This step can be very time consuming.",
                                    "Try using argument 'vfilter'.")
                        }
                        n.sv <- sva::num.sv(prot, design.mm, vfilter=vfilter)
                        if (n.sv > 0){
                            svobj <- sva::sva(prot, design.mm,
                                              design.mm[ , -2, drop=FALSE], n.sv=n.sv,
                                              vfilter=vfilter)
                            design.mm <- cbind(design.mm, svobj$sv)
                        }
                        rm(svobj, n.sv)
                        suppressMessages(gc())
                    }

                    # Fit the model
                    if (verbose){
                        message("Fitting the model.")
                    }
                    fit <- limma::lmFit(prot, design.mm, ...)
                    if(ebayes) {
                        fit <- limma::eBayes(fit)
                        tbl <- limma::topTable(fit, coef=2, n=Inf, p.value=1)
                    } else {
                        tbl <- limma::toptable(fit, coef=2, n=Inf, p.value=1)
                    }

                    # -----------------------------------------------------

                    list(
                        N=nrow(pheno),
                        error=NA,
                        design=design,
                        result=tbl
                    )
                }, error=function(e) {
                    list(
                        N=NA,
                        error=e,
                        design=NA,
                        result=NULL
                    )
                })

                results <- list("cluster"=results)
                new("ResultSet",
                    fun_origin = "assocPRT",
                    class_origin = c("ExposomeClust", "ExpressionSet"),
                    names = c(texp, tpro),
                    results = results,
                    fData = fData(object)[c(texp, tpro)],
                    sva=ifelse(sva, 1, 0)
                )
            }
            ## ------------------------------------------------------------- ##
        } else if(tann == "exposure") {
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
            results <- parallel::mclapply(select, function(ex) {
                design <- as.formula(paste0("~", ex, "+", formula[2]))
                pheno <- rexposome:::.create_p(
                    expo.dt = exp.dt,
                    omic.p = pData(object[[tpro]]),
                    select = all.vars(design)
                )
                if(class(pheno) == "character") {
                    stop("Invalid value '", pheno, "' in 'exposures' or 'covariates'")
                }
                if(sum(is.na(pheno)) != 0 & (warnings | verbose)) {
                    warning("Given design ('", design, "') has ",
                            sum(is.na(pheno)), " NA values")
                }

                na.loc <- rowSums(apply(pheno, 2, is.na))
                na.loc <- which(na.loc != 0)
                if(length(na.loc) != 0){
                    warning("There are missing values. ", length(na.loc),
                            " samples will be removed.")
                    pheno <- pheno[-na.loc, , drop=FALSE]
                }

                # -------------------------------------------------------------

                if(verbose) {
                    message("Testing '", ex, "' (", design, ")")
                }

                if(sum(!sapply(sapply(apply(pheno, 2, table), length), ">", 1))) {
                    warning("When testing for '", ex, "', at last one covariate ",
                            "is constant")
                    return(list(
                        N=NA,
                        design=NA,
                        result=NULL,
                        error="cavariate is constant"
                    ))
                } else {
                    tryCatch({
                        # Design model
                        design.mm <- model.matrix(formula(design), data = pheno)
                        prot <- object[[tpro]][ , rownames(pheno), drop=FALSE]

                        # If required, apply SVA
                        if(sva) {
                            if (verbose | warnings){
                                message("Computing SVA. This step can be very time consuming.",
                                        "Try using argument 'vfilter'.")
                            }
                            n.sv <- sva::num.sv(prot, design.mm, vfilter=vfilter)
                            if (n.sv > 0){
                                svobj <- sva::sva(prot, design.mm,
                                                  design.mm[ , -2], n.sv=n.sv,
                                                  vfilter=vfilter)
                                design.mm <- cbind(design.mm, svobj$sv)
                            }
                            rm(svobj, n.sv)
                            suppressMessages(gc())
                        }

                        # Fit the model
                        if (verbose){
                            message("Fitting the model.")
                        }
                        fit <- limma::lmFit(prot, design.mm, ...)
                        if(ebayes) {
                            fit <- limma::eBayes(fit)
                            tbl <- limma::topTable(fit, coef=2, n=Inf, p.value=1)
                        } else {
                            tbl <- limma::toptable(fit, coef=2, n=Inf, p.value=1)
                        }

                        # -----------------------------------------------------

                        list(
                            N=nrow(pheno),
                            error=NA,
                            design=design,
                            result=tbl
                        )
                    }, error=function(e){
                        message(e)
                        return(list(
                            N=NA,
                            error=e,
                            design=NA,
                            result=NULL
                        ))
                    })
                }
            }, mc.cores = ncores, mc.preschedule=FALSE)
            names(results) <- select

            new("ResultSet",
                fun_origin = "assocPRT",
                class_origin = c("ExposomeSet", "ExpressionSet"),
                names = c(texp, tpro),
                results = results,
                fData = fData(object)[c(texp, tpro)],
                sva=ifelse(sva, 1, 0)
            )
            ## ------------------------------------------------------------- ##
        } else {
            stop("Type of analysis could not be detected. Has 'MultiDataSet' an 'ExposomeSet' or 'ExposomeClus' object?")
        }

})
