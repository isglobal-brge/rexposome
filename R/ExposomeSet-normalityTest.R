#' @describeIn ExposomeSet Test the normality of each exposure.
#' @param exposure Vecror of exposures to be used.
#' @param th Threshold of P-Value used to considere normalit
#' @param min.val Minimum number of observations to perform test
#' @param na.rm If set to \code{TRUE} removes \code{NA} values
setMethod(
    f = "normalityTest",
    signature = "ExposomeSet",
    definition = function(object, exposure, th = 0.05, min.val = 5, na.rm = TRUE) {
        if(missing(exposure)) {
            exposure <- exposureNames(object)
        } else {
            if(sum(exposure %in% exposureNames(object)) != length(exposure)) {
                stop("Given exposures not in ExposomeSet")
            }
        }
        if(length(exposure) == 1) {
            expos <- assayDataElement(object, "exp")[exposure, ]
            pv <- shapiro.test(expos[!is.na(expos)])
            tst <- data.frame(exposure = exposure, normality = pv$p.value <= th, p.value = pv$p.value)
            rownames(tst) <- exposure
        } else {
            tst <- data.frame(do.call(rbind, apply(assayDataElement(object, "exp")[exposure, ], 1, function(ex) {
                ex <- ex[!is.na(ex)]
                if(sum(!is.na(ex)) >= min.val) {
                    pv <- shapiro.test(ex)
                    list(pv$p.value >= th, pv$p.value)
                } else {
                    pv <- NA
                    list(NA, NA)
                }
            })))
            tst$exposure <- exposure
            tst <- tst[ , c(3, 1, 2)]
            colnames(tst) <- c("exposure", "normality", "p.value")

            tst$p.value <- as.numeric(tst$p.value)
            tst$normality <- unlist(tst$normality)

            tst <- tst[order(tst$p.value), ]
        }
        tst
    }
)
