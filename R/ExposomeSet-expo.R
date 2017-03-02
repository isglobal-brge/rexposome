setMethod(
    f = "expos",
    signature = "ExposomeSet",
    definition = function(object) {
        expos <- t(assayDataElement(object, "exp"))
        type <- fData(exps)[colnames(expos), "_type"]
        for(ii in 1:length(type)) {
            expos[expos[ , ii] == "NA" , ii] <- NA
            if(type[ii] == "numeric") {
                expos[ , ii] <- as.numeric(expos[ , ii])
            } else {
                expos[ , ii] <- as.factor(expos[ , ii])
            }
        }
    }
)
