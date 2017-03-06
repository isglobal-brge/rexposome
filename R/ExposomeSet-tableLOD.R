#' @describeIn ExposomeSet Returns a vector with the number of under-LOD values
#' per exposure.
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
            x <- sapply(rownames(ld), function(ex) {
                if(is.na(ld[ex, lod.col])) {
                    return(0)
                } else {
                    sum(dta[ , ex] <= ld[ex, "LOD"], na.rm = TRUE)
                }
            })
        } else if(output == "p") {
            x <- sapply(rownames(ld), function(ex) {
                if(is.na(ld[ex, lod.col])) {
                    return(0)
                } else {
                    sum(dta[ , ex] <= ld[ex, "LOD"], na.rm = TRUE) / nrow(dta)
                }
            })
        } else {
            stop("Invalid 'output' type.")
        }

        if(sort) {
            x <- x[order(x, na.last = TRUE, decreasing = TRUE)]
        }

        return(x)
    }
)
