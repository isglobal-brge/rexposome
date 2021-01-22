#' @describeIn ExposomeSet Imputation of missing values of exposures.
# @param ssystem Method to be used to impute missing values (\code{"mice"} or
# \code{"hmisc"}).
#' @param messages If set to \code{TRUE} messages from  \code{mice}'s function
#' will be displayed.
setMethod(
    f = "imputation",
    signature = "ExposomeSet",
    definition = function(object, select, ..., messages = FALSE) {

        if(missing(select)) {
            select <- exposureNames(object)
        } else {
            if(sum(select %in% exposureNames(object)) != length(select)) {
                stop("Given exposures not in ExposomeSet (description).")
            }
        }

        dta <- expos(object)[ , select, drop = FALSE]
        imp <- do.call(cbind,
            lapply(colnames(dta), function(ex) { Hmisc::impute(dta[ , ex], ...)}))
        colnames(imp) <- colnames(dta)
        rownames(imp) <- rownames(dta)

        fData(object)$`.imp` <- "hmisc"

        select.no <- exposureNames(object)[!exposureNames(object) %in% select]

        imp <- cbind(imp, t(assayData(object)[["exp"]][select.no, ]))
        colnames(imp) <- c(select, select.no)
        imp <- imp[ , exposureNames(object)]

        assayData(object) <- assayDataNew("environment",
                                          exp = t(as.matrix(imp)))
        return(object)
    }
)
