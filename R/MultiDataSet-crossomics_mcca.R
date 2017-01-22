.crossomics_mcca <- function(object, ncomponents = 2, ..., na.rm = FALSE,
                          verbose = FALSE, warnings = TRUE) {
        ## Check for at last two sets
        if(length(names(object)) < 2) {
            stop("Input 'MultiDataSet' must contain at last 2 sets")
        }

        ## Check that we know how to get the data of the given sets
        names <- sapply(strsplit(names(object), "\\+"), "[[", 1)
        for(set in names) {
            if(!set %in% c("expression", "exposures", "snps", "methylation", "protein")){
                stop("Set of type '", set, "' not allowed")
            }
        }
        rm(names)
        # -------------------------------------------------------------------------

        if(warnings) {
            warning("Sets in 'MultiDataSet' will be reduced to common samples.")
        }
        object <- MultiDataSet:::commonSamples(object)

        # -------------------------------------------------------------------------

        ## Extract data as list. Convert to continuous if necesary.
        crossDta <- list()
        class <- c()

        for(set in names(object)) {
            set_type <- strsplit(set, "\\+")[[1]]
            if(set_type == "snps") {
                class <- c(class, "SnpSet")
                if(verbose) {
                    message("Creating table for SNPs (", set, ")")
                }
                crossDta[[set]] <- t(rexposome::snpToContinuous(object[[set]], verbose))
                rownames(crossDta[[set]]) <- colnames(object[[set]])
                colnames(crossDta[[set]]) <- rownames(object[[set]])
                if(sum(is.na(crossDta[[set]])) & !na.rm) {
                    stop("Table SNPs (", set, ") contains NA values.")
                } else {
                    if(verbose) {
                        message("Removing SNPs with missing genotype.")
                    }
                    onc <- ncol(crossDta[[set]])
                    crossDta[[set]] <- crossDta[[set]][ , colSums(is.na(crossDta[[set]])) == 0]
                    if(verbose | warnings) {
                        r <- onc - ncol(crossDta[[set]])
                        warning("Removed ", r, " (", round(r/onc * 100, 2), "%) SNPs from original SnpSet.")
                    }
                    rm(onc)

                    object@featureData[[set]] <- object@featureData[[set]][colnames(crossDta[[set]]), ]
                }
            } else if(set_type == "expression") {
                class <- c(class, "ExpressionSet")
                if(verbose) {
                    message("Creating table for Gene Expression (", set, ")")
                }
                crossDta[[set]] <- t(assayDataElement(object[[set]], "exprs"))
                if(sum(is.na(crossDta[[set]]))) {
                    stop("Table Expression (", set, ") contains NA values.")
                }
            } else if(set_type == "protein") {
                class <- c(class, "ExpressionSet")
                if(verbose) {
                    message("Creating table for Proteome (", set, ")")
                }
                crossDta[[set]] <- t(assayDataElement(object[[set]], "exprs"))
                if(sum(is.na(crossDta[[set]]))) {
                    stop("Table Proteome (", set, ") contains NA values.")
                }
            } else if(set_type == "methylation") {
                class <- c(class, "MethylationSet")
                if(verbose) {
                    message("Creating table for Methylation (", set, ")")
                }
                crossDta[[set]] <- t(assayDataElement(object[[set]], "meth"))
                if(sum(is.na(crossDta[[set]]))) {
                    stop("Table Methylation (", set, ") contains NA values.")
                }
            } else if(set_type == "exposures") {
                class <- c(class, "ExposomeSet")
                if(verbose) {
                    message("Creating table for Exposome (", set, ")")
                }
                xx <- as.data.frame(object[[set]], phe = FALSE) ## t(assayDataElement(object[[set]], "exp"))
                if(verbose | warnings) {
                    warning("Factor exposures will be discarded.")
                }
                yy <- data.frame(A = 1:nrow(xx))
                cl <- c()
                for(col in colnames(xx)) {
                    if(class(xx[, col]) != "factor") {
                        yy <- cbind(yy, xx[, col])
                        cl <- c(cl, col)
                    }
                }
                yy <- yy[ , -1]
                colnames(yy) <- cl;
                rownames(yy) <- rownames(xx); rm(cl, xx)
                crossDta[[set]] <- yy
                if(sum(is.na(crossDta[[set]]))) {
                    stop("Table Exposome (", set, ") contains NA values.")
                }
            } else {
                stop("Invalid set '", set, "' (", set_type,
                     "). Cannot get inner data")
            }
        }
        # -------------------------------------------------------------------------

        # Create the two tables and integrate them with MultiCCA
        if(verbose) {
            message("Performing crossomics")
        }

        int <- PMA::MultiCCA(crossDta, ncomponents = ncomponents, ...)
        # -------------------------------------------------------------------------

        ans <- new("ResultSet",
                   fun_origin = "crossomics",
                   class_origin = "<m:mcca>",
                   names = names(object),
                   results = list(list("result" = int)),
                   fData = fData(object)
        )

        if("SnpSet" %in% class) {
            tsnp <- grep("snps", names(object))
            tsnp <- names(object)[tsnp]

            ans@fData[[tsnp]] <- ans@fData[[tsnp]][colnames(crossDta[[tsnp]]), ]
        }

        if("ExposomeSet" %in% class) {
            texp <- grep("exposures", names(object))
            texp <- names(object)[texp]

            ans@fData[[texp]] <- ans@fData[[texp]][colnames(crossDta[[texp]]), ]
        }

        return(ans)
}

#
#
#     omicNames <- names(omicSets)
#
#     # Filter samples if necessary
#     sam <- lapply(omicSets, sampleNames)
#     sam[["exp"]] <- rexposome::samplesNames(object)
#     sel <- intersect2(sam)
#     rm(sam)
#
#     if(length(sel) == 0) {
#       stop("No common samples between 'ExposomeSet' and given omics datasets.")
#     } else if((length(sel) < 10) & (verbose | warnings)) {
#       warning("Less than 10 samples in common between 'ExposomeSet' and 'ExpressionSet'.")
#     }
#     omicSets <- lapply(omicSets, function(ds) ds[ , sel])
#     names(omicSets) <- omicNames
#
#     object <- object[ , sel, ]
#     # --------------------------------------------------------------------------
#
#     # Get data for crossomics
#     if(verbose) {
#       message("Creating tables and performing Sparse CCA (Multitable).")
#     }
#
#     crossDta <- list()
#     if("snp" %in% omicNames) {
#       # Update to continous variable
#       crossDta[["snp"]] <- t(rexposome::snpToContinuous(omicSets[["snp"]], verbose))
#       rownames(crossDta[["snp"]]) <- colnames(omicSets[["snp"]])
#       colnames(crossDta[["snp"]]) <- rownames(omicSets[["snp"]])
#       # --------------------------------------------------------------------------
#
#       # Remove NAs for SNP
#       if(verbose) {
#         message("Creating tables for SNPs.")
#       }
#
#       if(na.rm) {
#         if(verbose) {
#           message("Removing SNPs with missing genotype.")
#         }
#         onc <- ncol(crossDta[["snp"]])
#         crossDta[["snp"]] <- crossDta[["snp"]][ , colSums(is.na(crossDta[["snp"]])) == 0]
#         omicSets[["snp"]] <- omicSets[["snp"]][colnames(crossDta[["snp"]])[colSums(is.na(crossDta[["snp"]])) == 0], ]
#         if(verbose | warnings) {
#           warning("Removed ", onc - ncol(crossDta[["snp"]]), " SNPs from original SnpSet.")
#         }
#         rm(onc)
#       }
#     }
#     if("genexp" %in% omicNames) {
#       if(verbose) {
#         message("Creating tables for Gene Expression.")
#       }
#       crossDta[["genexp"]] <- t(assayDataElement(omicSets[["genexp"]], "exprs"))
#     }
#     if("methy" %in% omicNames) {
#       if(verbose) {
#         message("Creating tables for Methylation.")
#       }
#       crossDta[["methy"]] <- t(assayDataElement(omicSets[["methy"]], "meth"))
#     }
#
#     if(verbose) {
#       message("Creating tables for Exposures.")
#     }
#     crossDta[["exp"]] <- t(assayDataElement(object, "exp"))
#     # --------------------------------------------------------------------------
#
#     # Create the two tables and integrate them with MultiCCA
#     if(verbose) {
#       message("Performing crossomics.")
#     }
#
#     int <- PMA::MultiCCA(crossDta, ncomponents = ncomponents, ...)
#     # --------------------------------------------------------------------------
#
#     ans <- new("IntegrationSet",
#                class_origin = "ExposomeSet",
#                fun_origin = "crossomics",
#                design = ~ 1,
#                results = list("correlations" = int$ws)
#     )
#     ans <- rexposome::add.snps(ans, omicSets[["snp"]])
#     ans <- rexposome::add.genexp(ans, omicSets[["genexp"]])
#     ans <- rexposome::add.methy(ans, omicSets[["methy"]])
#     ans <- rexposome::add.exp(ans, object)
#   }
# )
#
#
# intersect2 <- function(...) {
#   args <- list(...)
#   nargs <- length(args)
#   if(nargs <= 1) {
#     if(nargs == 1 && is.list(args[[1]])) {
#       do.call("intersect2", args[[1]])
#     } else {
#       stop("cannot evaluate intersection fewer than 2 arguments")
#     }
#   } else if(nargs == 2) {
#     intersect(args[[1]], args[[2]])
#   } else {
#     intersect(args[[1]], intersect2(args[-1]))
#   }
# }
