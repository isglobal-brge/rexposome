#' @describeIn ExposomeSet Performs an EXposome-Wide Association Study (modelling the exposures as response)
#' @param formula Formula, not including exposures, to be tested. No need to provide response (left term)
#' @param filter Expression to be used to filter \code{\link{ExposomeSet}}
#' @param tef If \code{TRUE} it computed the threshold for effective tests.
#' @param verbose If set to \code{TRUE} is shows messages on progression.
#' @param warnings If set to \code{TRUE} it prints the warning messsages.
setMethod(
  f = "invExWAS",
  signature = "ExposomeSet",
  definition = function(object, formula, filter,
                        tef = TRUE, verbose = FALSE, warnings = TRUE){
    dta <- cbind(expos(object), pData(object))
    if(!missing(filter)) {
      sel <- eval(substitute(filter), as.data.frame(dta))
      dta <- dta[sel, ]
    }
    if(class(formula) == "character") {
      formula <- formula(formula)
    }
    results <- as.data.frame(t(sapply(exposureNames(object), function(ex){
      tryCatch({
        if(verbose) {
          message("Processing '", ex, "'.")
        }
        ff <- reformulate(attr(terms(formula), "term.labels"), response = ex)
        typ <- class(dta[,ex])
        if(typ == "factor"){
          mod <- nnet::multinom(ff, data=dta, model = FALSE, trace = FALSE)
        } 
        else {
          mod <- lm(ff, data=dta, model = FALSE)
        }
        if(warnings & !is.null(mod$na.action)){
          lost_rows <- length(mod$na.action)
          warning(lost_rows, " rows removed when evaluating '", ex, "'")
        }
        mod0 <- update(mod, as.formula(paste("~ -", attr(terms(ff), "term.labels")[1])), 
                       data=if(is.null(mod$na.action)){dta}else{dta[-mod$na.action,]})
        pp <- anova(mod0, mod)
        pvalue <- pp[2, ncol(pp)]
        effect <- c(coef(mod)[2],
                    confint.default(mod)[2,])
        return(c(effect, pvalue))
      }, error = function(e) {
        if(warnings) {
          message("[warning]: ", e)
        }
      })
    })))
    colnames(results) <- c("effect", "2.5","97.5", "pvalue")
    if(tef) {
      cormat <- extract(correlation(object,
                                    use="pairwise.complete.obs", method.cor = "pearson"))
      M <- ncol(cormat)
      lambdas <- base::eigen(cormat)$values
      Vobs <- sum(((lambdas - 1)^2)) / (M - 1)
      Meff <- M - sum((lambdas>1)*(lambdas-1))
      alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
    } else {
      alpha_corrected <- 0
    }
    new("ExWAS",
        effective = alpha_corrected,
        comparison = S4Vectors::DataFrame(results),
        description = S4Vectors::DataFrame(pData(featureData(object))),
        formula = reformulate(attr(terms(formula), "term.labels"), response = "expositions")
    )
  }
)