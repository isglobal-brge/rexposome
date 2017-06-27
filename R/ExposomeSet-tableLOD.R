#' @describeIn ExposomeSet Returns a vector with the number of under-LOD values
#' per exposure.
#' @param output Can take values \code{"n"} (count) ot \code{"p"} (percentage)
setMethod(
    f = "tableLOD",
    signature = "ExposomeSet",
    definition = function(object, output = "n", lod.col = "LOD", sort = TRUE) {
        ld <- fData(object)[ , c("Family",  lod.col)]
        dta <- t(assayDataElement(object, "exp"))

        ld <- ld[!is.na(ld$LOD), ]

        if(nrow(ld) <= 0) {
            stop("No exposures under LOD in given ExposomeSet.")
        }

        if(output == "n") {
            x <- vapply(rownames(ld), function(ex) {
                if(is.na(ld[ex, lod.col])) {
                    return(0)
                } else {
                    sum(dta[ , ex] <= ld[ex, "LOD"], na.rm = TRUE)
                }
            }, FUN.VALUE = numeric(1))
        } else if(output == "p") {
            x <- vapply(rownames(ld), function(ex) {
                if(is.na(ld[ex, lod.col])) {
                    return(0)
                } else {
                    sum(dta[ , ex] <= ld[ex, "LOD"], na.rm = TRUE) / nrow(dta)
                }
            }, FUN.VALUE = numeric(1))
        } else {
            stop("Invalid 'output' type.")
        }

        if(sort) {
            x <- x[order(x, na.last = TRUE, decreasing = TRUE)]
        }

        return(x)
    }
)
