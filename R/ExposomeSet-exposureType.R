# .exposureType <- function(object, exposure) {
#     exposure <- assayData(object)[["exp"]][exposure, ]
#
#     if(fData(object)[exposure, "_type"] == "numeric") {
#         return(exposure)
#     } else {
#         return(as.factor(exposure))
#     }
# }

# setMethod(
#     f = "exposureType",
#     signature = "ExposomeSet",
#     definition = function(object, exposure, asFactor, as.type = FALSE) {
#         exposure <- assayData(object)[["exp"]][exposure, ]
#         exposure.u <- unique(as.numeric(as.character(exposure)))
#
#         if(missing(asFactor)) {
#             asFactor = object@exp2fac
#         }
#
#         if(!as.type) {
#             if(length(exposure.u) >= asFactor) {
#                 return("numeric")
#             } else {
#                 return("factor")
#             }
#         } else {
#             if(length(exposure.u) >= asFactor) {
#                 return(exposure)
#             } else {
#                 return(as.factor(exposure))
#             }
#         }
#     }
# )
