#' @describeIn ExposomeSet Performs clustering on samples based on exposure levels.
#' @param object An \code{\link{ExposomeSet}} object.
#' @param method Method to be used.
#' @param cmethod Function implementing a systsme to retrieve classification
#' from clustering output
#' @param ... Argument sent given method.
#' @param warnings If set to \code{TRUE} shows intermediate warnings.
setMethod(
  f = "clustering",
  signature = "ExposomeSet",
  definition = function(object, method, cmethod, ..., warnings = TRUE) {
    if (warnings) {
      warning("Non continuous exposures will be discarded.")
    }

    if (!"data" %in% names(formals(method))) {
      stop("Invalid method for clustering analysis. Requested method with 'data' argument.")
    }

    select <- rownames(fData(object))[fData(object)$`_type` == "numeric"]

    data.clust <- expos(object)[ , select, drop = FALSE]

    method <- match.fun(method)

    mod.l <- list()
    mod.l$model <- method(data = data.clust, ...)

    method <- as.character(sys.calls()[[1]])[3]
    call <- deparse(sys.calls()[[1]])

    if(!missing(cmethod)){
        cl <- cmethod(mod.l$model)
    } else {
        cl <- mod.l$model$classification
    }

    return(new("ExposomeClust",
       assayData = assayDataNew("environment", exp = t(data.clust)),
       phenoData = AnnotatedDataFrame(cbind(pData(object)[rownames(data.clust), , drop=FALSE], cluster=cl)),
       featureData = featureData(object)[colnames(data.clust), ],
       model = mod.l,
       call = call,
       method = method,
       samples = sampleNames(object)
    ))

  })
