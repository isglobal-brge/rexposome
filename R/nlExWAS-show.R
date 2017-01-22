setMethod(
    f = "show",
    signature = "nlExWAS",
    definition = function(object) {
        cat("An object of class 'nlExWAS'\n\n")
        cat(" . Tested exposures: ", object@n, "\n")
        cat(" . Included phenotypes: ", ncol(object@X), "\n")
        if(sum(is.na(object@ranking$diffAIC)) == nrow(object@ranking)) {
            cat("\n  -> All models failed  <-\n")
        } else {
            cat("\n")
            print(head(object@ranking, n=5))
        }
    }
)
