#' @describeIn ExposomeSet Imputation of missing values of exposures.
# @param ssystem Method to be used to impute missing values (\code{"mice"} or
# \code{"hmisc"}).
#' @param messages If set to \code{TRUE} messages from  \code{mice}'s function
#' will be displayed.
setMethod(
    f = "imputation",
    signature = "ExposomeSet",
    definition = function(object, select, ..., messages = FALSE) {
        #ssystem <- match.arg(ssystem, c("mice", "hmisc"))
        ssystem <- "hmisc"

        if(missing(select)) {
            select <- exposureNames(object)
        } else {
            if(sum(select %in% exposureNames(object)) != length(select)) {
                stop("Given exposures not in ExposomeSet (description).")
            }
        }

        dta <- as.data.frame(t(assayData(object)[["exp"]][select, ]))

        if(ssystem == "mice") {
            # imp <- mice::mice(dta, printFlag = messages, ...)
            # fData(object)$`_imp` <- imp$method
            # imp <- mice::complete(imp)

        } else { #hmisc
            imp <- apply(dta, 2, function(row) { Hmisc::impute(row, ...)})
            fData(object)$`_imp` <- "hmisc"
        }

        select.no <- exposureNames(object)[!exposureNames(object) %in% select]

        imp <- cbind(imp, t(assayData(object)[["exp"]][select.no, ]))
        colnames(imp) <- c(select, select.no)
        imp <- imp[ , exposureNames(object)]

        assayData(object) <- assayDataNew("environment",
                                          raw = assayDataElement(object, "raw"),
                                          exp = t(as.matrix(imp)))
        return(object)
    }
)
