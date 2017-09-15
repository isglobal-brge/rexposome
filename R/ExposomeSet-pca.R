#' @describeIn ExposomeSet Performs a PCA
#' @param npc Number of PC to be kept
setMethod(
    f = "pca",
    signature = "ExposomeSet",
    definition = function(object, npc = 10) {
        select <- rownames(fData(object))[fData(object)$`.type` == "numeric"]
        exposures <- expos(object)[ , select]
        # exposures <- do.call(cbind, lapply(exposuresNames(object), function(ex) {
        #     if(exposureType(object, ex) == "numeric") {
        #         rwn <<- c(rwn, ex)
        #         as.numeric(exposureType(object, ex, TRUE))
        #     }
        # }))
        # colnames(exposures) <- rwn
        # rownames(exposures) <- sampleNames(object)

        pca_expo <- FactoMineR::PCA(exposures,
            scale.unit = FALSE,
            ncp = npc,
            graph = FALSE
        )

        class(pca_expo) <- "list"

        ans <- new("ExposomePCA",
            assayData = assayDataNew("environment", exp = t(exposures)),
            featureData = featureData(object)[select, ],
            phenoData = phenoData(object),
            pca = pca_expo
        )
    }
)
