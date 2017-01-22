.crossomics_mcia <- function(object, ncomponents = 2, ...,
                             verbose = FALSE, warnings = TRUE) {
    ## Check for at last two sets
    if(length(names(object)) < 2) {
        stop("Input 'MultiDataSet' must contain at last 2 sets")
    }

    if(warnings | verbose) {
        warning("Sets from 'MultiDataSet' will be reduced to common samples")
    }

    l1 <- sapply(sampleNames(object), length)
    object <- MultiDataSet:::commonSamples(object)
    l2 <- sapply(sampleNames(object), length)
    l3 <- mapply('-', l1, l2, SIMPLIFY = FALSE)

    if(verbose) {
        message(paste(unlist(l3), names(l3),
                      sep = " samples were reduced from ", collapse = ", "))
    }

    mres <- omicade4::mcia(as_list(object), cia.nf = ncomponents, ...)
    ans <- new("ResultSet",
               fun_origin = "crossomics",
               class_origin = "<m:mcia>",
               names = names(object),
               results = list(list("result" = mres)),
               fData = fData(object)
    )

}

as_list <- function(x) {
    ll <- lapply(names(x), function(dtype) {
        elm <- assayDataElementNames(x[[dtype]])[1]
        assayDataElement(x[[dtype]], elm)
    })
    names(ll) <- names(x)
    return(ll)
}
