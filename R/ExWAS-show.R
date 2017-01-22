setMethod(
    f = "show",
    signature = "ExWAS",
    definition = function(object) {
        cat("An object of class 'ExWAS'\n\n")
        ff <- as.character(object@formula)
        cat("      ",ff[2], ff[1], gsub(" ", "", ff[3]), "\n\n")
        cat("Tested exposures: ", nrow(object@comparison), "\n")
        cat("Threshold for effective tests (TEF): ",
            format(object@effective, digits = 3, scientific = TRUE), "\n")
        cat(" . Tests < TEF:", sum(object@comparison$pvalue < tef(object)), "\n")
    }
)
