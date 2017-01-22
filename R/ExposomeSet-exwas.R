setMethod(
    f = "exwas",
    signature = "ExposomeSet",
    definition = function(object, formula, filter, family, ..., verbose = FALSE, warnings = TRUE) {

        dta <- as.data.frame(object)
        if(!missing(filter)) {
            sel <- eval(substitute(filter), as.data.frame(object))
            dta <- dta[sel, ]
        }

        if(family == "binomial") {
            test <- "Chi"
        } else if(family == "gaussian") {
            test <- "F"
        } else {
            test <- "LRT"
        }

        #form <- match.call(exwas, sys.call(sys.parent()), expand.dots=FALSE)$...$formula
        #dta <- as.data.frame(object)
        form <- as.character(formula)
        ne <- c()
        items <- lapply(exposureNames(object), function(ex) {
            if(verbose) {
                message("Processing '", ex, "'.")
            }

            frm <- as.formula(paste0(form[2], "~", ex, "+", form[3]))

            nr <- nrow(dta)
            for(phe in all.vars(frm)) {
                dta <- dta[!is.na(dta[ , phe]), ]
            }
            dta <- dta[ , all.vars(frm)]

            if(warnings & nrow(dta) - nr != 0) {
                warning("Data from '", paste0(form[2], "~", ex, "+", form[3]), "' was reduced from ", nr, " to ", nrow(dta))
            }

            if(warnings & nrow(dta) == 0) {
                stop("Obtanied 0 samples for test '", paste0(form[2], "~", ex, "+", form[3]), "'")
            }
            tryCatch({
                mod <- glm(..., family=family, formula = frm, data = dta)
                mod0 <- update(mod, as.formula(paste0(". ~ . - ", all.vars(frm)[2])))
                effect <- c(mod$coef[2], suppressMessages(confint.default(mod)[2,]))
                p <- anova(mod, mod0, test = test)
                p2 <- p[[names(p)[length(names(p))]]][2] # `Pr(>F)`, `Pr(>Chi)`
                return(c(effect, p2))
            }, error = function(e) {
                effect <- NULL
                p2 <- NULL
                ne <<- c(ne, ex)
                return(c(effect, p2))
            })
        })

        if(length(ne) != 0) {
            warning("The association of some exposures (", length(ne), ") could not be evaluated. Their effect and p-value were set to NULL.")
        }

        items <- as.data.frame(do.call(rbind, lapply(items, function(xx) {
            ans <- t(data.frame(xx))
            colnames(ans) <- c("effect", "2.5","97.5", "pvalue")
            ans
        })))
        rownames(items) <- exposureNames(object)
        #items$fdr <- p.adjust(items$pvalue, method="fdr")

        ## Compute the threshold for effective tests
        cormat <- extract(correlation(object,
            use="pairwise.complete.obs", method.cor = "pearson"))
        M <- ncol(cormat)
        lambdas <- base::eigen(cormat)$values
        Vobs <- sum(((lambdas - 1)^2)) / (M - 1)
        Meff <- M - sum((lambdas>1)*(lambdas-1))
        alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
        ## /

        new("ExWAS",
            effective = alpha_corrected,
            comparison = items,
            description = pData(featureData(object)),
            formula = formula
        )
    }
)

# a <- function(object, phenotype, covariates, verbose = FALSE,
#                         warnings = TRUE) {
#     if(missing(phenotype)) {
#       warning("No given 'phenotype'. EWAS will be computed for all phenotypes.")
#       phenotype <- phenotypesNames(object)
#     }
#     if(!missing(covariates)) {
#       if(sum(c(phenotype, covariates) %in% phenotypesNames(object)) != length(c(phenotype, covariates))) {
#         stop("Not all phenotypes in ExposomeSet.")
#       }
#     } else {
#       covariates <- c()
#     }
#
#     cvnames <- paste(covariates, collapse="+")
#     if(nchar(cvnames) != 0) {
#       fr <- paste0("~exp+", cvnames)
#     } else {
#       fr <- paste0("~exp")
#     }
#
#     items <- lapply(phenotype, function(phe) {
#       type <- .pheno_type(object, phe)
#       lapply(exposureNames(object), function(exp) {
#         form <- paste0(phe, fr)
#         data <- .create_p(t(assayDataElement(object, "exp")), pData(object),
#                           data.frame(), c(exp, covariates, phe))
#         colnames(data) <- c("exp", covariates, phe)
#         nr <- nrow(data)
#
#         data <- data[!is.na(data[ , phe]), ]
#         data <- data[!is.na(data[ , "exp"]), ]
#
#         if(verbose) {
#             message("Data from '", phe, "~", exp, "' was reduced from ", nr,
#                     " to ", nrow(data), " (type: ", type, ")")
#         }
#
#         if(type == "numeric") {
#           data[, phe] <- as.numeric(as.character(data[, phe]))
#           mod <- glm(form, family = "gaussian", data = data)
#           mod0 <- update(mod, . ~ . - exp )
#           effect <- c(mod$coef[2], suppressMessages(confint(mod)[2,]))
#           p <- anova(mod, mod0, test = "F")
#           p2 <- p$`Pr(>F)`[2]
#         } else if(type == "factor") {
#           data[, phe] <- as.factor(as.character(data[, phe]))
#           mod <- glm(form, family = "binomial", data = data)
#           mod0 <- update(mod, . ~ . - exp)
#           effect <- exp(c(mod$coef[2], suppressMessages(confint(mod)[2,])))
#           p <- anova(mod, mod0, test = "Chi")
#           p2 <- p$`Pr(>Chi)`[2]
#         } else if(type == "multi.factor") { # más de dos
#           stop("Invalid type of factor (multifactor)")
#         } else { #(tyoe == "lod)
#           stop("Invalid type of factor (LoD).")
#         }
#
#         ans <- c(effect, p2)
#         return(ans)
#       })
#     })
#     items <- lapply(items, function(xx) {
#       ans <- t(data.frame(xx))
#       colnames(ans) <- c("exp", "2.5","97.5", "pvalue")
#       rownames(ans) <- exposureNames(object)
#       ans
#     })
#     names(items) <- phenotype
#
#     new("EWAS",
#         comparison = items,
#         description = pData(featureData(object)),
#         phenotype = phenotype,
#         desc.famCol = object@desc.famCol
#     )
#   }
