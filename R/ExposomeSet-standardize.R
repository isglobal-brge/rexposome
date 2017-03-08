#' @describeIn ExposomeSet Standardization of exposures.
setMethod(
    f = "standardize",
    signature = "ExposomeSet",
    definition = function(object, select, method = c("normal", "robust"),
        na.rm = TRUE, warnings = TRUE) {
        method <- match.arg(method, choices = c("normal", "robust"))
        if(missing(select)) {
            select <- exposureNames(object)
        }
        select.no <- exposureNames(object)[!exposureNames(object) %in% select]

        if(sum(fData(object)[select, "_type"] == "factor") != 0) {
            if(warnings) {
                warning("Given categorical exposures.")
            }
            select.no <- c(select.no,
                select[fData(object)[select, "_type"] == "factor"]
            )
            select <- select[
                fData(object)[select, "_type"] != "factor"
            ]
        }

        if(warnings) {
            warning("Categorical exposures will not be standardized.")
        }

        dd <- expos(object)[ , select, drop=FALSE]
        if (method == "normal") {
            center <- apply(dd, 2, mean, na.rm = na.rm)
            vari   <- apply(dd, 2, sd, na.rm = na.rm)
        } else if (method == "robust") {
            center <- apply(dd, 2, median, na.rm = na.rm)
            vari   <- apply(dd, 2, mad, na.rm = na.rm)
        } else {
            stop("Invalid method for standardize.")
        }

        dd <- apply(dd, 2, function(x) as.numeric(as.character(x)))
        dd <- scale(dd, center = center, scale = vari)
        dd <- cbind(dd,
            t(assayData(object)[["exp"]][select.no, ]))

        assayData(object) <- assayDataNew("environment",
                                          raw = assayDataElement(object, "raw"),
                                          exp = t(dd)[rownames(assayDataElement(object, "raw")), ])
        fData(object)[select, "_std"] <- method

        return(object)
      }
)
