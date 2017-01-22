#' @describeIn nlExWAS Method to obtain the matrix of AIC scores
#' @param sort If \code{TRUE}, the results are ordered by AIC
#' @aliases nlExWAS-methods
setMethod(
    f = "extract",
    signature = "nlExWAS",
    definition = function(object, sort = TRUE) {
        x <- object@ranking
        if(sort) {
            x <- x[order(x$AIC, na.last = TRUE), ]
        }
        x
    }
)
