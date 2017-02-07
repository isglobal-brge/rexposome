.plot_assoc_genexp <- function(object, rid, type, tFC=1.5, tPV=-log10(0.001), ...) {
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

    if(type == "qq") {
        ##qqman::qq(object@results[[rid]]$result$P.Value, ...)
        plot.qq(object@results[[rid]]$result$P.Value)
    }

    if(type == "manhattan") {
        dta <- object@results[[rid]]$result[ ,
            c("probeset_id",  "P.Value", "chromosome", "start")]
        colnames(dta) <- c("SNP", "P", "CHR", "BP")
        dta$CHR <- gsub("chr", "", sapply(strsplit(dta$CHR, "_"), "[[", 1))
        dta$CHR <- gsub("X", "23", gsub("Y", "24", dta$CHR))
        dta <- dta[dta$CHR %in% as.character(1:24), ]
        dta$CHR <- as.numeric(dta$CHR)
        dta$BP <- as.numeric(dta$BP)

        qqman::manhattan(dta, ylab="-log10(P.Value)", ...)
    }

    if(type == "volcano") {
        title <- ifelse(class(rid) == "character", rid, names(object@results)[rid])

        dta <- object@results[[rid]]$result
        dta$clr <- "gray87"
        dta$alp <- 0.2
        dta$PV <- -log10(dta$P.Value)
        dta$feature <- rownames(dta)

        dta$clr[dta$PV >= tPV] <- "tan3"
        dta$alp[dta$PV >= tPV] <- 0.4
        dta$clr[abs(dta$logFC) >= tFC] <- "olivedrab"
        dta$alp[abs(dta$logFC) >= tFC] <- 0.4

        dta$clr[dta$PV >= tPV & abs(dta$logFC) >= tFC] <- "lightskyblue"
        dta$alp[dta$PV >= tPV & abs(dta$logFC) >= tFC] <- 0.9

        clrvalues <- c("gray87", "tan3", "olivedrab", "lightskyblue")
        names(clrvalues) <- c("gray87", "tan3", "olivedrab", "lightskyblue")

        plt <- ggplot2::ggplot(dta, ggplot2::aes(x=logFC, y=PV, color=clr, fill=clr, alpha=alp)) +
            ggplot2::theme_bw() +
            ggplot2::geom_point() +
            ggplot2::geom_hline(yintercept=tPV, linetype="dotdash", color="gray69", size=0.75) +
            ggplot2::geom_vline(xintercept=c(-tFC, tFC), linetype="dotdash", color="gray69", size=0.75) +
            ggplot2::scale_colour_manual(values=clrvalues) +
            ggplot2::ggtitle(paste0("Gene Expression - ", title, " Association\nVolcano Plot")) +
            ggplot2::xlab("Log Fold Change") +
            ggplot2::ylab("-log10(P.Value)") +
            ggplot2::theme(legend.position="none") +
            ggrepel::geom_text_repel(
                data = subset(dta, dta$PV >= tPV & abs(dta$logFC) >= tFC),
                ggplot2::aes(logFC, PV, label=feature),
                size = 2,
                box.padding = ggplot2::unit(0.35, "lines"),
                point.padding = ggplot2::unit(0.3, "lines"),
                color="black"
            )
        return(plt)
    }
}

