#' @describeIn mExWAS Method to obtain the generated model
#' @param sort NOT USED
#' @aliases mExWAS-methods
setMethod(
    f = "extract",
    signature = "mExWAS",
    definition = function(object, sort = TRUE) {
        return(as.data.frame(as.matrix(stats::coef(object@result[[1]], c(object@result[[1]]$lambda.min, object@result[[1]]$lambda.1se)))))
    }
)

#' @describeIn mExWAS Method to obtain raw model
#' @param sort NOT USED
#' @aliases mExWAS-methods
setMethod(
    f = "raw",
    signature = "mExWAS",
    definition = function(object) {
        return(object@result[[2]])
    }
)

