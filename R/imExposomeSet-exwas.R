#' @describeIn imExposomeSet Performs an EXposome-Wide Association Study
#' @param formula Formula, not including exposures, to be tested.
#' @param filter Expression to be used to filter \code{\link{ExposomeSet}}
#' @param family Family descriving the nature of the health outcome
#' @param verbose If set to \code{TRUE} is shows messages on progression.
setMethod(
    f = "exwas",
    signature = "imExposomeSet",
    definition = function(object, formula, filter, family, ..., verbose = FALSE, warnings = TRUE) {
        dta <- cbind(expos(object), pData(object))
        if(!missing(filter)) {
            sel <- eval(substitute(filter), as.data.frame(dta))
            dta <- dta[sel, ]
        }

        if(class(formula) == "character") {
            formula <- formula(formula)
        }

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
                warning("Data from '", paste0(form[2], "~", ex, "+", form[3]), "' was reduced from ", nr, " to ", nrow(dta), "\n")
            }

            if(warnings & nrow(dta) == 0) {
                stop("Obtanied 0 samples for test '", paste0(form[2], "~", ex, "+", form[3]), "'")
            }
            tryCatch({
                dta <- expos(object)
                dta <- cbind(dta, pData(object)[rownames(dta), -(1:2)])
                ## TEST
                fit_glm <- lapply(1:object@nimputation, function(ii) {
                    dtai <- dta[dta[, 1] == ii, ]
                    tst <- glm(family=family, formula = frm, data = dtai)
                })
                tst <- rexposome:::pool_glm(fit_glm, m = object@nimputation)
                ## NULL
                fit_glm <- lapply(1:object@nimputation, function(ii) {
                    dtai <- dta[dta[, 1] == ii, ]
                    tst <- glm(family=family, formula = frm, data = dtai)
                })
                tst <- rexposome:::pool_glm(fit_glm, m = object@nimputation)

                summary(tst)[2, c(1, 6, 7, 5)]
            }, error = function(e) {
                ne <<- c(ne, ex)
                return(c(NULL, NULL, NULL, NULL))
            })
        })

        if(length(ne) != 0) {
            warning("The association of some exposures (", length(ne), ") could not be evaluated. Their effect and p-value were set to NULL.")
        }

        items <- data.frame(do.call(rbind, items))
        colnames(items) <- c("effect", "2.5","97.5", "pvalue")
        rownames(items) <- exposureNames(object)

        new("ExWAS",
            effective = NaN,
            comparison = items,
            description = fData(object),
            formula = formula
        )
    }
)
