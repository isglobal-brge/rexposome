#' @describeIn ExposomeSet Standardization of exposures.
setMethod(
    f = "standardize",
    signature = "ExposomeSet",
    definition = function(object, select, method = "normal",
        na.rm = TRUE, warnings = TRUE) {
        method <- match.arg(method, choices = c("normal", "iqr", "iqr2", "robust"))
        if(missing(select)) {
            select <- exposureNames(object)
        }
        select.no <- exposureNames(object)[!exposureNames(object) %in% select]

        if(sum(fData(object)[select, ".type"] == "factor") != 0) {
            if(warnings) {
                warning("Given categorical exposures.")
            }
            select.no <- c(select.no,
                select[fData(object)[select, ".type"] == "factor"]
            )
            select <- select[
                fData(object)[select, ".type"] != "factor"
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
        } else if (method == "iqr") {
            center <- apply(dd, 2, median, na.rm = na.rm)
            vari   <- apply(dd, 2, IQR, na.rm = na.rm)
            iqr.normal.range <- qnorm(0.75)-qnorm(0.25)
            vari   <- vari/iqr.normal.range
        } else if (method == "iqr2") {
            center <- apply(dd, 2, mean, na.rm = na.rm)
            vari <- apply(dd, 2, IQR, na.rm = na.rm)
        } else {
            stop("Invalid method for standardize.")
        }

        dd <- apply(dd, 2, function(x) as.numeric(as.character(x)))
        dd <- scale(dd, center = center, scale = vari)
        dd <- cbind(dd,
            t(assayData(object)[["exp"]][select.no, ]))

        assayData(object) <- assayDataNew("environment",
            exp = t(dd)[rownames(assayDataElement(object, "exp")), ])
        fData(object)[select, ".std"] <- method

        return(object)
      }
)
