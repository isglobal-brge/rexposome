#' @describeIn mExWAS Method to obtain the generated model
#' @param sort NOT USED
setMethod(
    f = "extract",
    signature = "mExWAS",
    definition = function(object, type = "test", sort = TRUE) {
        type <- match.arg(type, choices = c("test", "raw"))
        if(type == "test") {
            return(as.data.frame(as.matrix(stats::coef(object@result[[1]], c(object@result[[1]]$lambda.min, object@result[[1]]$lambda.1se)))))
        } else if(type == "raw") {
            return(object@result[[2]])
        }
    }
)
