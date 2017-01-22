setMethod(
    f = "extract",
    signature = "ResultSet",
    definition = function(object, rid, sort = TRUE) {
        # ## checking -----------------------------------------------------------
        # if(missing(rid)) {
        #     rid <- 1:length(object@results)
        # }
        # if(class(rid) == "numeric") {
        #     if(rid < 1 | rid > length(object@results)) {
        #         stop("Invalid 'rid'. IT must be greather than 1 an lower than ",
        #              length(object@results))
        #     }
        # } else {
        #     if(!rid %in% names(object@results)) {
        #         stop("Given 'rid' (", rid, ") not in results.")
        #     }
        # }
        # ## --------------------------------------------------------------------

        if(object@fun_origin == "assocGE") {
            if(missing(rid)) {
                res <- lapply(names(object@results), function(nme) {
                    tt <- object@results[[nme]]$result
                    tt$exposure <- nme
                    return(tt)
                })
                res <- do.call(rbind, res)
            } else {
                res <- object@results[[rid]]$result$expression
            }
        } else if(object@fun_origin == "assocME") {
            if(missing(rid)) {
                res <- lapply(names(object@results), function(nme) {
                    tt <- object@results[[nme]]$result
                    tt$exposure <- nme
                    colnames(tt)[2] <- "effect"
                    return(tt)
                })
                res <- do.call(rbind, res)
            } else {
                res <- object@results[[rid]]$result
            }
        } else if(object@fun_origin == "assocSNP") {
            if(!missing(rid)) {
                warning("Given 'rid'. Invalid argument for assocSNP result.")
            }
            res <- object@results[[1]]$result
            res <- res[order(res$PValHWE), ]
        } else if(object@fun_origin == "crossomics") {
            res <- data.frame(do.call(rbind, lapply(1:length(names(object)), function(ii) {
                tbl <- cbind(object@results[[1]][[1]]$ws[[ii]], names(object)[ii])
                rownames(tbl) <- rownames(object@fData[[ii]])
                colnames(tbl) <- c("x", "y", "feature")
                tbl[ tbl[ , 1] != 0 | tbl[ , 2] != 0, ]
            })), stringsAsFactors = FALSE)
            res$x <- as.numeric(res$x)
            res$y <- as.numeric(res$y)
        } else if(object@fun_origin == "assocPRT") {
            if(missing(rid)) {
                res <- lapply(names(object@results), function(nme) {
                    tt <- object@results[[nme]]$result
                    tt$exposure <- nme
                    tt
                })
                res <- do.call(rbind, res)
            } else {
                res <- object@results[[rid]]$result
            }
        } else {
            stop("Invalid 'object'. Value for attribue 'fun_origin' (",
                 object@fun_origin, ") not recognized.")
        }
        return(res)
})
