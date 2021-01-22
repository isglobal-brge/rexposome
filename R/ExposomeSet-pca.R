#' @describeIn ExposomeSet Performs a PCA
#' @param npc Number of PC to be kept
#' @param pca Perform PCA (only numerical variables) or FAMD (numerical and categorical)
#' @param ... Arguments to be passed to imputeFAMD
setMethod(
    f = "pca",
    signature = "ExposomeSet",
    definition = function(object, npc = 10, pca = FALSE, ...) {
        if(pca == TRUE){
            select <- rownames(fData(object))[fData(object)$`.type` == "numeric"]
            exposures <- expos(object)[ , select]
            pca_expo <- FactoMineR::PCA(exposures,
                scale.unit = FALSE,
                ncp = npc,
                graph = FALSE
            )
        }
        else{
            select <- rownames(fData(object))
            exposures <- expos(object)
            if(any(is.na(exposures))){
                warning("There are missings on the exposome dataset, 
                        'missMDA::imputeFAMD' is used")
                impute <- missMDA::imputeFAMD(exposures, ncp = npc, ...)
                pca_expo <- FactoMineR::FAMD(exposures,
                                             ncp = npc,
                                             graph = FALSE,
                                             tab.disj = impute$tab.disj)
            }
            else{
                pca_expo <- FactoMineR::FAMD(exposures,
                                             ncp = npc,
                                             graph = FALSE)
            }
        }

        class(pca_expo) <- "list"

        ans <- new("ExposomePCA",
            assayData = assayDataNew("environment", exp = t(exposures)),
            featureData = featureData(object)[select, ],
            phenoData = phenoData(object),
            pca = pca_expo
        )
    }
)
