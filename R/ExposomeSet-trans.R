#' @describeIn ExposomeSet Transformation of exposures.
#' @param fun Function to bt used in the transformation process
setMethod(
  f = "trans",
  signature = "ExposomeSet",
  definition = function(object, fun, select, by.exposure=FALSE, ...) {
    if(missing(fun)) {
      stop("Invalid content for 'fun' argument.")
    }
    if(missing(select)) {
      select <- exposureNames(object)
    }
    if(sum(select %in% exposureNames(object)) != length(select)) {
        stop("Slected exposures not in given ExposomeSet.")
    }
    select.no <- exposureNames(object)[!exposureNames(object) %in% select]
    data.cnt <- expos(object)[ , select, drop=FALSE] # t(assayData(object)[["exp"]][select, , drop=FALSE])
    if(by.exposure) {
        data.dst <- data.frame(lapply(colnames(data.cnt), function(exp) {
          fun(data.cnt[ , exp], ...)
        }))
    } else {
        data.dst <- fun(data.cnt, ...)
    }

    data.dst <- cbind(data.dst,
                      t(assayData(object)[["exp"]][select.no, ]))
    colnames(data.dst) <- c(select, select.no)

    assayData(object) <- assayDataNew("environment",
        exp = t(data.dst)[rownames(assayDataElement(object, "exp")), ])
    tryCatch({
        fData(object)[select, ".trn"] <- as.character(substitute(fun))
    }, error=function(e) {
        fData(object)[select, ".trn"] <- "unknown"
    })

    return(object)
  }
)
