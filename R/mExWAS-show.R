setMethod(
    f = "show",
    signature = "mExWAS",
    definition = function(object) {
        cat("An object of class 'mExWAS'\n\n")
        cat(" . Phenotype: ", object@phenotype, "\n")
        if(object@method == "enet") {
            cat(" . #Exposures: ", object@mresult[[1]]$dim[1], "\n")
        } else {
            cat(" . #Exposures: ", length(object@mresult[[1]]$var.names), "\n")
        }
    }
)
