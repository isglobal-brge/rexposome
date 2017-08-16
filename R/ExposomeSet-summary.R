#' @describeIn ExposomeSet  Summary of both continuous and categorical exposures
setMethod(
    f = "Summary",
    signature = "ExposomeSet",
    #definition = function(object, set=c("exposures", "phenotypes"), select) {
    definition = function(x, set=c("exposures", "phenotypes"), select, ...,
                          na.rm = FALSE) {
        set <- match.arg(set)
        if(missing(select)) {
            select <- switch(set,
                exposures = exposureNames(x),
                phenotypes = phenotypeNames(x)
            )
        } else {
            ss <- sum(select %in% switch(set,
                     exposures = exposureNames(x),
                     phenotypes = phenotypeNames(x)
                    ))
            if(ss != length(select)) {
                stop("Selected exposures/phenotypes not in ExpressionSet.")
            }
        }
        switch(set,
            exposures = .summary_exposures(x, select),
            phenotypes = summary(pData(phenoData(x))[ , select, drop=FALSE])
        )
    }
)

.summary_exposures <- function(object, select) {
    exposome <- expos(object)[ , select, drop=FALSE]
    type <- fData(object)[select, ".type"]
    for(ii in seq(length(type))) {
        message(ii)
        exposome[, ii] <- switch (type[ii],
            numeric = as.numeric(as.character(exposome[, ii])),
            factor = as.factor(as.character(exposome[, ii]))
        )
    }
    exposome <- exposome
    summary(exposome)
}
