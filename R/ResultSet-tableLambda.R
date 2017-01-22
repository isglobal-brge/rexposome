setMethod(
    f = "tableLambda",
    signature = "ResultSet",
    definition = function(object) {
        data.frame(
            "exposure"=unique(rid(object)),
            "lambda"=sapply(rid(object), function(expo) {
                qchisq(median(extract(object, rid=expo)$P.Value), df=2, lower.tail=FALSE)
            })
        )
    }
)
