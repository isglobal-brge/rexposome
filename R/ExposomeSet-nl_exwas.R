setMethod(
    f = "nl_exwas",
    signature = "ExposomeSet",
    definition = function(object, phenotype, bbs.df, ..., mc.cores = 1, verbose = FALSE, warnings = TRUE) {
        # Fit gamboost to all outcomes of expressions of
        # an expressionSet (eSet class) or Y data.frame
        # using all covariates (exposions) of X data.frame
        Y <- t(assayDataElement(object, "exp"))
        phen <- pData(object)

        if(verbose) {
            message("Loading package 'mboost'.")
            suppressMessages(require(mboost))
        }

        if(!missing(phenotype)) {
            if(sum(phenotype %in% colnames(phen)) != length(phenotype)) {
                stop("Not all given phenotypes are in the ExposomeSet's pData")
            }

            phen <- phen[ , phenotype, drop = FALSE]
        }

        do.factors <- function(x, lm = 5) {
            if(is.factor(x) | length(levels(as.factor(x))) >= lm)
                res <- x
            else if(!is.factor(x))
                res <- as.factor(x)
            res
        }
        X <- do.call(cbind.data.frame, lapply(phen, do.factors))
        xcol.factor <- unlist(lapply(phen, is.factor))

        rm(phen)

        Y.names <- colnames(Y)
        xformula <- ifelse(xcol.factor,
            paste0("bols( ", names(xcol.factor), " )"),
            paste0("bbs( ", names(xcol.factor), " , df = ", bbs.df," )")
        )

        if(verbose) {
            message("Testing ", nrow(Y), " subjects, ", ncol(Y), " exposures and ", ncol(X), " phenotypes.")
        }

        ## --------------------------------------------------------------------
        faux <- function(ii) {
            xyformula <- as.formula(paste(Y.names[ii], " ~ ",
                paste0(xformula, collapse = " + ")))
            data <- cbind(Y[ , ii], X)
            colnames(data)[1] <- Y.names[ii]
            #model <- try(gamboost(xyformula, data = data, ...), TRUE)

            model <- try(gamboost(xyformula, data = data), TRUE)

            if(class(model)[1] != "try-error") {
                aic <- AIC(model, df = "trace")
                optimal <- attr(aic, "mstop")
                df <- as.double(attr(aic, "df")[optimal])
                cor2 <- as.double(cor(model$response, predict(model))^2)
                predictors <- mboost::extract(model, "variable.names")
                p <- length(predictors)
                model <- model[optimal]
            } else {
                model <- NULL
                predictors <- NULL
                aic <- NA
                df <- NA
                cor2 <- NA
                p <- NA
            }
            NUL <- try(lm(as.formula(paste(Y.names[ii], " ~ 1")), data = data),
                       TRUE)
            if(class(NUL)[1] != "try-error") {
                aicNUL <- try(AIC(NUL), TRUE)
                if(class(aicNUL)[1] != "try-error") {
                    diffAIC <- aicNUL[1] - aic[1]
                }
            } else {
                diffAIC <- NA
            }


#             if (return.mod) {
#                 res <- list(model = model, predictors = predictors,
#                             AIC = data.frame(AIC = aic[1], df = df[1], p = p,
#                                              pseudoR2 = cor2, diffAIC = diffAIC))
#             } else {
            res <- list(model = model, predictors = predictors,
                AIC = data.frame(AIC = aic[1], df = df[1], p = p,
                pseudoR2 = cor2, diffAIC = diffAIC)
            )
            # }
            return(res)
        }
        ## --------------------------------------------------------------------

        results <- parallel::mclapply(1:ncol(Y), faux, mc.cores = mc.cores)
        xyformula <- sapply(1:ncol(Y), function(ii) {
            as.formula(paste(Y.names[ii], " ~ ", paste0(xformula, collapse = " + ")))
        })

        #results <- parallel::mclapply(1:10, faux, mc.cores = 1)

#         if (return.mod) {
             models <- lapply(results, function(x) x$model)
             names(models) <- Y.names
#         } else  {
#        models <- NULL
        # }

        predictors <- lapply(results, function(x) x$predictors)
        aic <- do.call(rbind, lapply(results, function(x) x$AIC))
        aic$outcome <- names(predictors) <- Y.names
        aic <- aic[order(aic$diffAIC, decreasing = TRUE), c(6,1:5)]
        rownames(aic) <- NULL
        ans <- new("nlExWAS", ranking = aic, models = models, n = ncol(Y), predictors = predictors,
                        X = X, xyformula = xyformula)
        #class(results) <- "gamboostFit"
        #results
        return(ans)
    }
)
#
# print.gamboostFit <- function(x, N.top = 25, ...) {
#     cat("\n")
#     if(x$n < N.top | is.infinite(N.top)) N.top <- x$n
#     if(sum(is.na(x$ranking$diffAIC)) == nrow(x$ranking)) {
#         print("All models failed.\n")
#     } else {
#         exclude <- sum(x$ranking$diffAIC[1:N.top] < 0 )
#         if( exclude > 0)
#             print(x$ranking[1:(N.top - exclude),])
#         else
#             print(x$ranking[1:N.top,])
#     }
# }
