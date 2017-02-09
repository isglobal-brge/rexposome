.plot_assoc_snps <- function(object, type, feature, ...) {
    if(type == "qq") {
        #qqman::qq(object@results[[1]]$test$p.value, ...)
        qq_plot(object@results[[1]]$test$p.value)
    } else if(type == "manhattan") {
        if(nrow(object@fData[[1]]) == 0) {
            stop("Invalid call to 'GWAS' plot. No 'fData' given to original SnpSet.")
        }
        if(sum(c("chromosome", "position") %in% tolower(colnames(object@fData[[2]]))) != 2){
            stop("Provided 'fData' does not contain 'chromosome' nor 'position'.")
        }
        cchr=which("chromosome" == tolower(colnames(object@fData[[2]])))
        cpos=which("position" == tolower(colnames(object@fData[[2]])))
        dta <- object@results[[1]]$result
        rownames(dta) <- dta$Name
        colnames(dta)[2] <- "P"
        colnames(dta)[1] <- "SNP"
        dta$CHR <- pData(object@fData[[2]])[dta$SNP, cchr]
        dta$BP <-  pData(object@fData[[2]])[dta$SNP, cpos]
        dta$CHR <- gsub("chr", "", dta$CHR)
        dta <- dta[!is.na(dta$CHR) , c("SNP", "P", "CHR", "BP")]
        dta$CHR <- gsub("X", "23", dta$CHR)
        dta$CHR <- gsub("Y", "24", dta$CHR)
        dta <- dta[dta$CHR %in% as.character(1:24), ]
        dta$CHR <- as.numeric(dta$CHR)
        dta$BP <- as.numeric(dta$BP)
        dta <- dta[!is.na(dta$P), ]
        suppressWarnings({
            qqman::manhattan(dta, ylab="-log10(P.Value)", ...)
        })
    } else {
        stop("Invalid type '", type, "'.")
    }
}
