#' @describeIn ExposomeSet Returns a vector with the number of missing values
#' per exposure.
setMethod(
    f = "tableMissings",
    signature = "ExposomeSet",
    definition = function(object, set = c("exposures", "phenotypes"),
            output = "n", sort = TRUE) {
        if(set == "exposures") {
            dta <- t(assayDataElement(object, "exp"))
        } else if(set == "phenotypes") {
            dta <- pData(object)
        } else {
            stop("Invalid 'set' selected.")
        }

        if(output == "n") {
            x <- apply(dta, MARGIN=2, function(colm) sum(is.na(colm)))
        } else if(output == "p") {
            x <- apply(dta, MARGIN=2, function(colm) sum(is.na(colm)) * 1.0 / length(colm))
        } else {
            stop("Invalid 'output' type.")
        }

        if(sort) {
            x <- x[order(x)]
        }

        return(x)
    }
)
