#' @describeIn ExposomeSet Test the normality of each exposure.
#' @param exposure Vecror of exposures to be used.
#' @param th Threshold of P-Value used to considere normalit
#' @param min.val Minimum number of observations to perform test
#' @param na.rm If set to \code{TRUE} removes \code{NA} values
setMethod(
    f = "normalityTest",
    signature = "ExposomeSet",
    definition = function(object, exposure, th = 0.05, min.val = 5,
                          na.rm = TRUE, warnings = TRUE) {
        if(missing(exposure)) {
            exposure <- exposureNames(object)
            exposure <- exposure[
                fData(object)[exposure, ".type"] == "numeric"
            ]
        } else {
            if(sum(exposure %in% exposureNames(object)) != length(exposure)) {
                stop("Given exposures not in 'ExposomeSet'.")
            }
            if(sum(exposure[fData(object)[exposure, ".type"] == "factor"])
               != 0) {
                stop("Given categorical exposures.")
            }
        }


        dta <- expos(object)
        if(nrow(dta) > 4999) {
            if(warnings) {
                warning("Given 'ExposomeSet' has more than 4999 samples. ",
                        "In order to compute 'shapiro.test' it will be ",
                        "reduced to random 4999 samples.")
            }
            dta <- dta[sample(1:nrow(dta), size = 4999), , drop = FALSE]
        }

        dta <- dta[ , fData(object)[exposure, ".type"] == "numeric"]

        if(length(exposure) == 1) {
            expos <- dta[ , exposure, drop = TRUE]
            pv <- shapiro.test(expos[!is.na(expos)])
            tst <- data.frame(exposure = exposure,
                              normality = pv$p.value <= th,
                              p.value = pv$p.value)
            rownames(tst) <- exposure
        } else {
            tst <- list()
            for(ex in exposure) {
                var <- dta[ , ex, drop = TRUE]
                if(sum(!is.na(var)) >= min.val) {
                    var <- var[!is.na(var)]
                    pv <- shapiro.test(var)
                    tst[[ex]] <- list(pv$p.value >= th, pv$p.value)
                } else {
                    tst[[ex]] <- list(NA, NA)
                }
            }
            tst <- data.frame(do.call(rbind, tst))
            tst$exposure <- exposure
            tst <- tst[ , c(3, 1, 2)]
            colnames(tst) <- c("exposure", "normality", "p.value")

            tst$p.value <- as.numeric(tst$p.value)
            tst$normality <- unlist(tst$normality)

            tst <- tst[order(tst$p.value), ]
        }
        return(tst)
    }
)
