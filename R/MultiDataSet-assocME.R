setMethod(
    f = "assocME",
    signature = "MultiDataSet",
    definition = function(object, formula, select, set="exposures",
                          area.test=FALSE, method="ls", betas=TRUE,
                          ..., sva=FALSE, vfilter=NULL, ncores=1, verbose=FALSE,
                          warnings=TRUE) {
        ## ----------------------------------------------------------------- ##
        ## CHEKS
        ## ----------------------------------------------------------------- ##
        set <- match.arg(set, choices=c("exposures", "phenotypes"))

        tmth <- grep("methylation", names(object))
        texp <- grep("exposures", names(object))
        tcls <- grep("cluster", names(object))
        tmth <- names(object)[tmth]

        if(length(texp) != 0 & length(tcls) != 0) {
            stop("Given 'MultiDataSet' contains both 'ExposomeSet' and 'ExposomeClust'.")
        }

        tann <- ifelse(length(texp) == 0, "cluster", "exposure")
        texp <- ifelse(length(texp) == 0, names(object)[tcls], names(object)[texp])
        rm(tcls)

        if(length(tmth) != 1) {
            stop("One of the tables must exists in 'MultiDataSet' as ",
                "'methylation'")
        }
        if(length(texp) != 1) {
            stop("One of the tables must exists in 'MultiDataSet' as ",
                "'exposures'")
        }
        if(verbose) {
            message("The following tables will be used in the association ",
                "process: ", paste0("'", paste(c(tmth, texp), collapse="', '"),
            "'"))
        }

        if(warnings | verbose) {
            warning("Sets from 'MultiDataSet' will be reduced to common samples")
        }

        l1 <- sapply(sampleNames(object)[c(tmth, texp)], length)
        object <- MultiDataSet::commonSamples(object)
        l2 <- sapply(sampleNames(object)[c(tmth, texp)], length)
        l3 <- mapply('-', l1, l2, SIMPLIFY = FALSE)

        if(verbose) {
            message(paste(unlist(l3), names(l3),
                sep = " samples were reduced from ", collapse = ", "))
        }
        ## ----------------------------------------------------------------- ##

        formula <- as.character(as.formula(formula))
        exp.dt <- as.data.frame(object[[texp]])
        fd <- object@featureData[[tmth]]@data
        if(tann == "cluster") {
            ## ------------------------------------------------------------- ##
            ## EXPOSOME CLUSTER ANALYSIS
            ## ------------------------------------------------------------- ##
            if(warnings | verbose) {
                warning("Given 'MultiDataSet' contains an 'ExposomeClust'. Association test will be performed on clustering result.")
            }
            design <- as.formula(paste0("~cluster+", formula[2]))
            pheno <- .create_p(
                expo.dt = exp.dt,
                omic.p = pData(object[[tmth]]),
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
                    class_origin = c("ExposomeClust", "MethylationSet"),
                    names = c(texp, tmth),
                    results = list("cluster"=list(
                            N=NA,
                            design=NA,
                            result=NULL,
                            error="covariate is constant"
                        )),
                    fData = fData(object)[c(texp, tmth)],
                    sva=ifelse(sva, 1, 0)
                )
            } else {
                results <- tryCatch({
                    # Design model
                    design.mm <- model.matrix(formula(design), data = pheno)

                    # Extract betas or Ms
                    if(betas) {
                        methy <- betas(object[[tmth]][ , rownames(pheno)])
                    } else {
                        methy <- getMs(object[[tmth]][ , rownames(pheno)])
                    }

                    # Filter missing outcome samples
                    #if(length(na.loc) != 0) {
                    #    methy <- methy[ , -na.loc, drop=FALSE]
                    #}

                    # If required, apply SVA
                    if(sva) {
                        if (verbose | warnings){
                            message("Computing SVA. This step can be very time consuming.")
                        }
                        n.sv <- sva::num.sv(methy, design.mm, vfilter=vfilter)
                        if (n.sv > 0){
                            svobj <- sva::sva(methy, design.mm,
                                              design.mm[ , -2, drop=FALSE], n.sv=n.sv,
                                              vfilter=vfilter)
                            design.mm <- cbind(design.mm, svobj$sv)
                        }
                        rm(svobj, n.sv)
                        suppressMessages(gc())
                    }

                    # Test the model
                    if(area.test) {
                        result <- MEAL::DARegion(set=methy, model=design.mm,
                                                 methods=c("blockFinder", "bumphunter", "DMRcate"),
                                                 ...)
                    } else {
                        result <- MEAL::DAProbe(set=methy, model=design.mm,
                                                method=method, ...)
                    }
                    # -----------------------------------------------------
                    suppressMessages(gc())

                    result <- cbind(result, fd[rownames(result), ])
                    list(
                        N=nrow(pheno),
                        design=design,
                        result=result,
                        error=NA
                    )
                }, error=function(e){
                    return(list(
                        N=NA,
                        design=NA,
                        result=NULL,
                        error=e
                    ))
                })
                results <- list("cluster"=results)
                new("ResultSet",
                    fun_origin = "assocME",
                    class_origin = c("ExposomeClust", "MethylationSet"),
                    names = c(texp, tmth),
                    results = results,
                    fData = fData(object)[c(texp, tmth)],
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
            ## results <- parallel::mclapply(select, function(ex) {
            results <- lapply(select, function(ex) {
                design <- as.formula(paste0("~", ex, "+", formula[2]))
                pheno <- .create_p(
                    expo.dt = exp.dt,
                    omic.p = pData(object[[tmth]]),
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
                        # Design model for association to classification
                        design.mm <- model.matrix(formula(design), data = pheno)

                        # Extract betas or Ms
                        if(betas) {
                            methy <- betas(object[[tmth]][ , rownames(pheno)])
                        } else {
                            methy <- getMs(object[[tmth]][ , rownames(pheno)])
                        }

                        # Filter missing outcome samples
                        #if(length(na.loc) != 0) {
                        #    methy <- methy[ , -na.loc]
                        #}

                        methy <- methy[ , rownames(pheno)]

                        # If required, apply SVA
                        if(sva) {
                            if (verbose | warnings){
                                message("Computing SVA. This step can be very time consuming.")
                            }
                            n.sv <- sva::num.sv(methy, design.mm, vfilter=vfilter)
                            if (n.sv > 0){
                                svobj <- sva::sva(methy, design.mm,
                                                  design.mm[ , -2], n.sv=n.sv,
                                                  vfilter=vfilter)
                                design.mm <- cbind(design.mm, svobj$sv)
                            }
                            rm(svobj, n.sv)
                            suppressMessages(gc())
                        }

                        # Test the model
                        if(area.test) {
                            result <<- MEAL::DARegion(set= methy, model=design.mm,
                                methods=c("blockFinder", "bumphunter", "DMRcate"),
                                ...)
                        } else {
                            result <- MEAL::DAProbe(set=methy, model=design.mm,
                                method=method, ...)
                        }
                        # -----------------------------------------------------
                        suppressMessages(gc())


                        result <- cbind(result, fd[rownames(result), ])
                        list(
                            N=nrow(pheno),
                            design=design,
                            result=result,
                            error=NA
                        )
                    }, error=function(e){
                        return(list(
                            N=NA,
                            design=NA,
                            result=NULL,
                            error=e
                        ))
                    })
                }
            })
            #}, mc.cores=ncores, mc.preschedule=FALSE)
            names(results) <- select

        new("ResultSet",
            fun_origin = "assocME",
            class_origin = c("ExposomeSet", "MethylationSet"),
            names = c(texp, tmth),
            results = results,
            fData = fData(object)[c(texp, tmth)],
            sva=ifelse(sva, 1, 0)
        )

        ## ------------------------------------------------------------- ##
        } else {
            stop("Type of analysis could not be detected. Has 'MultiDataSet' an 'ExposomeSet' or 'ExposomeClus' object?")
        }
})
