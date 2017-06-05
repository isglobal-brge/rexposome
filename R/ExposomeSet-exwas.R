#' @describeIn ExposomeSet Performs an EXposome-Wide Association Study
#' @param formula Formula, not including exposures, to be tested.
#' @param filter Expression to be used to filter \code{\link{ExposomeSet}}
#' @param family Family descriving the nature of the health outcome
#' @param tef If \code{TRUE} it computed the threshold for effective tests.
#' @param verbose If set to \code{TRUE} is shows messages on progression.
setMethod(
    f = "exwas",
    signature = "ExposomeSet",
    definition = function(object, formula, filter, family, ...,  tef = TRUE, verbose = FALSE, warnings = TRUE) {
        dta <- cbind(expos(object), pData(object))
        if(!missing(filter)) {
            sel <- eval(substitute(filter), as.data.frame(dta))
            dta <- dta[sel, ]
        }

        if(family == "binomial") {
            test <- "Chi"
        } else if(family == "gaussian") {
            test <- "F"
        } else {
            test <- "LRT"
        }

        if(class(formula) == "character") {
            formula <- formula(formula)
        }

        form <- as.character(formula)
        ne <- c()
        items <- rbind(1:4)
        colnames(items) <- c("effect", "2.5","97.5", "pvalue")
        dta_all <- dta
        for(ex in exposureNames(object)) {
            dta <- dta_all
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
                mod <- stats::glm(family=family, formula = frm, data = dta)
                mod0 <- update(mod, as.formula(paste0(". ~ . - ", all.vars(frm)[2])))
                effect <- c(mod$coef[2], suppressMessages(confint.default(mod)[2,]))
                p <- anova(mod, mod0, test = test)
                p2 <- p[[names(p)[length(names(p))]]][2] # `Pr(>F)`, `Pr(>Chi)`
                items <- rbind(items, c(effect, p2))
            }, error = function(e) {
                effect <- NULL
                p2 <- NULL
                ne <- c(ne, ex)
                items <- rbind(items, c(effect, p2))
            })
        }

        if(length(ne) != 0) {
            warning("The association of some exposures (", length(ne), ") could not be evaluated. Their effect and p-value were set to NULL.")
        }

        items <- as.data.frame(items)
        items <- items[-1, ]
        rownames(items) <- exposureNames(object)

        ## Compute the threshold for effective tests
        if(tef) {
            cormat <- psygenet2r::extract(correlation(object,
                                          use="pairwise.complete.obs", method.cor = "pearson"))
            M <- ncol(cormat)
            lambdas <- base::eigen(cormat)$values
            Vobs <- sum(((lambdas - 1)^2)) / (M - 1)
            Meff <- M - sum((lambdas>1)*(lambdas-1))
            alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
        } else {
            alpha_corrected <- 0
        }
        ## /

        new("ExWAS",
            effective = alpha_corrected,
            comparison = S4Vectors::DataFrame(items),
            description = S4Vectors::DataFrame(pData(featureData(object))),
            formula = formula
        )
    }
)
