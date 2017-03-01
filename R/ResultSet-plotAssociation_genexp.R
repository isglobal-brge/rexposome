.plot_assoc_genexp <- function(object, rid, coef, type, tFC=1.5, tPV=-log10(0.001),
                               id.col="probeset_id", pv.col="P.Value",
                               chr.col="seqname", pos.col="start", ...) {
    ## checking ---------------------------------------------------------------
    # if(type == "feature" & missing(feature)) {
    #     stop("For feature plot, argument 'feature' must be given")
    # }
    if(class(rid) == "numeric") {
        if(rid < 1 | rid > length(object@results)) {
            stop("Invalid 'rid'. It must be greather than 1 an lower than ",
                 length(object@results))
        } else {
            rid <- names(object@results)[rid]
        }
    } else {
        if(!rid %in% names(object@results)) {
            stop("Given 'rid' (", rid, ") not in results.")
        }
    }
    ## ------------------------------------------------------------------------

    dta <- limma::topTable(object@results[[rid]]$result, coef=coef, n=Inf)
    if(type == "qq") {
        ##qqman::qq(object@results[[rid]]$result$P.Value, ...)
        qq_plot(dta$P.Value)
    } else if(type == "manhattan") {
        dta <- dta[ ,
            c(id.col, pv.col, chr.col, pos.col)]
        colnames(dta) <- c("SNP", "P", "CHR", "BP")
        dta$CHR <- gsub("chr", "", sapply(strsplit(dta$CHR, "_"), "[[", 1))
        dta$CHR <- gsub("X", "23", gsub("Y", "24", dta$CHR))
        dta <- dta[dta$CHR %in% as.character(1:24), ]
        dta$CHR <- as.numeric(dta$CHR)
        dta$BP <- as.numeric(dta$BP)

        qqman::manhattan(dta, ylab="-log10(P.Value)", ...)
    } else if(type == "volcano") {
        volcano_plot(
            pval=dta$P.Value,
            fc=dta$logFC,
            names=rownames(dta),
            tFC=tFC,
            tPV=tPV
        )
    } else {
        stop("Invalid type of plot ('", type, "').")
    }
}

