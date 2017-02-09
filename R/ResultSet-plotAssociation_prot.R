.plot_assoc_prot <- function(object, rid, type, feature, main, ...) {
    ## checking ---------------------------------------------------------------
    if(type == "feature" & missing(feature)) {
        stop("For feature plot, argument 'feature' must be given")
    }
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
        qq_plot(object@results[[rid]]$result$protein$P.Value)
    } else if(type == "manhattan") {
        title <- ifelse(class(rid) == "character", rid, names(object@results)[rid])

        dta <- extract(object, rid=rid)
        dta$protein <- rownames(dta)
        plt <- ggplot2::ggplot(dta, ggplot2::aes(x=protein,
                                                 y=-log10(P.Value),
                                                 size=-log10(P.Value),
                                                 fill=-log10(P.Value),
                                                 color=-log10(P.Value)
                                                 )) +
            ggplot2::theme_bw() +
            ggplot2::geom_point(alpha=0.7) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                legend.position = "none"
            ) +
            ggplot2::xlab("") + ggplot2::ylab("-log10(P.Value)") +
            scale_colour_gradientn(
                colours = c("darkgray", "darkblue")) +
            ggplot2::ggtitle(paste0("Protein - ", title, " Association\nManhattan Plot"))
        if(!missing(main)) {
            plt <- plt + ggplot2::ggtitle(main)
        }
        return(plt)
    } else {
        stop("Invalid type of plot ('", type, "').")
    }

}

