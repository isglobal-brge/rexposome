setMethod(
    f = "tableLambda",
    signature = "ResultSet",
    definition = function(object, trim=0.5) {
        data.frame(
            "exposure"=unique(rid(object)),
            "lambda"=sapply(rid(object), function(expo) {
                lambdaClayton(extract(object, rid=expo)$P.Value, trim=trim)
                #qchisq(median(extract(object, rid=expo)$P.Value), df=2, lower.tail=FALSE)
            })
        )
    }
)
