#' @describeIn ExposomeSet  Summary of both continuous and categorical exposures
setMethod(
    f = "Summary",
    signature = "ExposomeSet",
    definition = function(object, set=c("exposures", "phenotypes"), select) {
        set <- match.arg(set)
        if(missing(select)) {
            select <- switch(set,
                exposures = rownames(assayData(object)[["exp"]]),
                phenotypes = colnames(pData(phenoData(object)))
            )
        } else {
            ss <- sum(select %in% switch(set,
                     exposures = rownames(assayData(object)[["exp"]]),
                     phenotypes = colnames(pData(phenoData(object)))
                    ))
            if(ss != length(select)) {
                stop("Selected exposures/phenotypes not in ExpressionSet.")
            }
        }
        switch(set,
            exposures = .summary_exposures(object, select),
            phenotypes = summary(pData(phenoData(object))[ , select, drop=FALSE])
        )
    }
)

.summary_exposures <- function(object, select) {
    exposome <- expos(object)[ , select, drop=FALSE]
    type <- fData(object)[select, "_type"]
    for(ii in 1:length(type)) {
        exposome[, ii] <- switch (type[ii],
            numeric = as.numeric(as.character(exposome[, ii])),
            factor = as.factor(as.character(exposome[, ii]))
        )
    }
    exposome <- exposome
    summary(exposome)
}
