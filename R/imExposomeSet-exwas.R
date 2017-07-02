#' @describeIn imExposomeSet Performs an EXposome-Wide Association Study
#' @param object An object of class \code{\link{imExposomeSet}}
#' @param formula Formula, not including exposures, to be tested.
#' @param filter Expression to be used to filter \code{\link{ExposomeSet}}
#' @param family Family descriving the nature of the health outcome
#' @param ... Other used arguments
#' @param tef If set to \code{TRUE} the threshold for effective
#' test is computed.
#' @param verbose If set to \code{TRUE} it shows messages on progression.
#' @param warnings If set to \code{TRUE} it shows warnings on progession.
setMethod(
    f = "exwas",
    signature = "imExposomeSet",
    definition = function(object, formula, filter, family, ..., tef = TRUE,
                          verbose = FALSE, warnings = TRUE) {
        if(missing(family)) {
            stop("Missing argument 'family'.")
        }
        dta <- data.frame(expos(object), pData(object)[ , -c(1:2)])
        nmis <- apply(dta[dta$`.imp` == 0, ], 2, function(x) sum(is.na(x)))
        dta <- dta[dta$`.imp` != 0, ]
        dta <- dta[ , -2]

        if(!missing(filter)) {
            sel <- eval(substitute(filter), as.data.frame(dta))
            dta <- dta[sel, ]
        }

        if(class(formula) == "character") {
            formula <- formula(formula)
        }

        form <- as.character(formula)

        ne <- list()
        items <- list()
        for(ex in exposureNames(object)) {
            if(verbose) {
                message("Processing '", ex, "'.")
            }

            frm <- as.formula(paste0(form[2], "~", ex, "+", form[3]))

            tryCatch({
                ## TEST
                fit_glm <- lapply(1:object@nimputation, function(ii) {
                    dtai <- dta[dta[, 1] == ii, -1]
                    stats::glm(family=family, formula = frm, data = dtai)
                })
                mira_glm <- list(
                    call = NULL,
                    call1 = NULL,
                    nmis = nmis,
                    analyses = fit_glm
                )
                class(mira_glm) <- "mira"
                tst <- pool_glm(mira_glm)

                items[[ex]] <- summary(tst)[2, c(1, 6, 7, 5)]
            }, error = function(e) {
                if(verbose) {
                    message("\tProcess of '", ex, "' failed.", e)
                }
                ne[[ex]] <- e
                items[[ex]] <- c(NULL, NULL, NULL, NULL)
            })
        }

        if(length(ne) != 0) {
            warning("The association of some exposures (", length(ne), ") could not be evaluated. Their effect and p-value were set to NULL.")
        }

        items <- data.frame(do.call(rbind, items))
        colnames(items) <- c("effect", "2.5","97.5", "pvalue")
        rownames(items) <- exposureNames(object)

        ## Compute the threshold for effective tests
        if(tef) {
            cormat <- psygenet2r::extract(correlation(toES(object, rid=1),
                use="pairwise.complete.obs", method.cor = "pearson"))
            M <- ncol(cormat)
            lambdas <- base::eigen(cormat)$values
            Vobs <- sum(((lambdas - 1)^2)) / (M - 1)
            Meff <- M - sum((lambdas>1)*(lambdas-1))
            alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
        } else {
            alpha_corrected <- -1
        }
        ## /

        new("ExWAS",
            effective = alpha_corrected,
            comparison = S4Vectors::DataFrame(items),
            description = fData(object),
            formula = formula
        )
    }
)
