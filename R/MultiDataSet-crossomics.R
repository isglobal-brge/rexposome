setMethod(
    f = "crossomics",
    signature = "MultiDataSet",
    definition = function(object, method = c("mcca", "mcia"), ncomponents = 2, ..., na.rm = FALSE,
                          verbose = FALSE, warnings = TRUE) {
        method <- match.arg(method)
        if(method == "mcca") {
            .crossomics_mcca(object, ncomponents=ncomponents, na.rm=na.rm,
                             verbose=verbose, warnings=warnings, ...)
        } else if(method == "mcia") {
            .crossomics_mcia(object, verbose=verbose, warnings=warnings, ...)
        } else {
            stop("Invalid method (", method, ") was given.")
        }
    }
)
