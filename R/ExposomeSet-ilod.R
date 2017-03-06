#' @describeIn ExposomeSet Imputation of under-LOD values of exposures.
setMethod(
    f = "ilod",
    signature = "ExposomeSet",
    definition = function(object, seed = 1234, lod.col = "LOD", pNA = 0.2, tLog = FALSE, method = "QRILC", warnings = TRUE, ...) {
        set.seed(seed)

        dta <- assayDataElement(object, "exp")
        fdt <- fData(object)

        if(!lod.col %in% colnames(fData(object))) {
            stop("The featureData of the given ExposomeSet has no column called '", lod.col, "'")
        }

        if(warnings) {
            message(sum(!is.na(fdt[ , lod.col])), " exposures will be checked for under-LOD imputation. ",
                    nrow(dta), " exposures will be involved in the process.")
        }
        ## SAVE CURRETN NAs FOR LATER RESTORE
        dta.rm <- do.call(rbind, lapply(rownames(dta), function(ex) {
            is.na(dta[ex, ])
        }))
        rownames(dta.rm) <- rownames(dta)
        ## /

        ## FILL UNDER-LOD VALUES BY NAs
        dta.na <- do.call(rbind, lapply(rownames(dta), function(ex) {
            if(!is.na(fdt[ex, lod.col])) {
                dta[ex, dta[ex, ] <= fdt[ex, lod.col]] <- NA
            }
            dta[ex, ]
        }))
        rownames(dta.na) <- rownames(dta)
        ## /

        ## LOG THE EXPOSURES IF REQUESTED
        if(tLog) {
            dta.na <- tryCatch({log(dta.na)}, error = function(err) {
                stop("Cannot apply log transformation on given ExposomeSet.")
            })
        }
        ## /

        ## CHECK NUMBER OF UNDER LOD VALUES OR MISSING VALUES
        pM <- apply(dta.na, 1, function(row) sum(is.na(row))/ncol(dta.na) > pNA)
        if(sum(pM) > 0) {
            stop("There are exposures with more under LOD values (or missing values) than allowed (",
                  pNA, "). Those exposures are: ", paste(rownames(dta.na)[pM], collapse=", "))
        }
        ## /

        ## IMPUTATION
        method <- match.arg(method, choices=c("QRILC", "MinProb"))
        suppressMessages({
            if(method == "MinProb") {
                dta.imp <- imputeLCMD::impute.MinProb(dta.na, ...)
            } else {
                dta.imp <- imputeLCMD::impute.QRILC(dta.na)[[1]]
            }
        })
        ## /

        ## RESTORE ORIGINAL NAs
        dta.vl <- do.call(rbind, lapply(rownames(dta.imp), function(ex) {
            dta.imp[ex, ][dta.rm[ex, ]] <- NA
        }))
        ## /

        # INVERSE LOG TRANSOFMRATION
        if(tLog) {
            dta.vl <- apply(dta.vl, 2, exp)
        }
        ## /

        assayData(object) <- assayDataNew("environment",
                                          raw = assayDataElement(object, "raw"),
                                          exp = dta.imp)
        return(object)
    }

)


# # exprsDataObj = generate.ExpressionData(nSamples1 = 6, nSamples2 = 6,
# #                                        meanSamples = 0, sdSamples = 0.2,
# #                                        nFeatures = 1000, nFeaturesUp = 50, nFeaturesDown = 50,
# #                                        meanDynRange = 20, sdDynRange = 1,
# #                                        meanDiffAbund = 1, sdDiffAbund = 0.2)
# # exprsData = exprsDataObj[[1]]
# #
# # # insert 15% missing data with 100% missing not at random
# # m.THR = quantile(exprsData, probs = 0.15)
# # sd.THR = 0.1
# # MNAR.rate = 50
# # exprsData.MD.obj = insertMVs(exprsData,m.THR,sd.THR,MNAR.rate)
# # exprsData.MD = exprsData.MD.obj[[2]]
