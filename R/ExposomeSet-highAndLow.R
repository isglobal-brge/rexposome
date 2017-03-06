#' @describeIn ExposomeSet Performs a discretization of continuous exposures.
setMethod(
  f = "highAndLow",
  signature = "ExposomeSet",
  definition = function(object, ngroups = 3,
                        intervals = c("standard", "extreme"), select,
                        drop = FALSE, warnings = TRUE) {
    intervals <- match.arg(intervals)
    if (warnings) {
      warning("Non continuous exposures will be keept but not transformed")
    }
    if(ngroups < 2) {
      stop("Invalid argument 'ngroups'. It must bé equal or greather than 2")
    }

    ## SEELCT NUMERICAL EXPOSURES
    if(missing(select)) {
        select.all <- fData(object)$`_type`
        names(select.all) <- exposureNames(object)
        select <- names(select.all[select.all == "numeric"])
    } else {
        select.all <- fData(object)$`_type`
        names(select.all) <- exposureNames(object)
        if(sum(select.all[select] == "numeric") != length(select)) {
            stop("Exposures in 'select' are not numerical.")
        }
    }
    ## /

    ## COVNERT FROM CNT TO DST
    data.cnt <- t(assayDataElement(object, "exp")[select, ])
    data.dst <- data.frame(lapply(colnames(data.cnt), function(exp) {
      gtools::quantcut(data.cnt[ , exp], seq(0, 1, length = ngroups + 1), right = FALSE)
    }))
    ##

    ## IF EXTREM PLACE NA TO MIDLE GROUPS
    if(intervals == "extreme") {
      lapply(colnames(data.dst), function(exp) {
        lvl <- levels(data.dst[ , exp])
        data.dst[!(data.dst[ , exp] %in% c(max(lvl), min(lvl))) , exp] <<- NA
      })
        trs <- "EXT"
    } else {
        trs <- "HaL"
    }
    ## /

    if(drop) {
        select.all <- names(select.all)
        select.no <- select.all[!select.all %in% select]
        data.dst <- cbind(data.dst,
                      t(assayData(object)[["exp"]][select.no, ]))
        patata <<- data.dst
        colnames(data.dst) <- c(select, select.no)
        data.dst <- data.dst[ , rownames(assayDataElement(object, "raw"))]
        assayData(object) <- assayDataNew("environment",
            exp = t(data.dst),
            raw = assayDataElement(object, "raw")
        )

        nfData <- fData(object)
        nfData[select, "_type"] <- "factor"
        nfData[select, "_fct"] <- trs
        fData(object) <- nfData

    } else {

        ## PLACE NEW NAMES TO EXPOSURES
        colnames(data.dst) <- paste(select, ".HL", sep = "")
        rownames(data.dst) <- rownames(data.cnt)
        ## /

        assayData(object) <- assayDataNew("environment",
            exp = rbind(assayDataElement(object, "exp"), t(data.dst)),
            raw = rbind(assayDataElement(object, "raw"), t(data.dst))
        )

        nfData <- fData(object)[select, ]
        rownames(nfData) <- colnames(data.dst)
        nfData$`_type` <- "factor"
        nfData$`_fct` <- trs
        fData(object) <- rbind(fData(object), nfData)
    }



    object
  }
)
