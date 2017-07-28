# @return A \code{data.frame} with all exposures and all phenotypes in an
# \link{ExposomeSet}.
#
# @param x Must be an \link{ExposomeSet}.
# @param row.names NOT USED
# @param optional NOT USED
# @param phe (default \code{TRUE}) If \code{TRUE} resulting \code{data.frame}
# includes both exposures and phenotypes.
# @param ... NOT USED
# @rdname as.data.frame
# @method as.data.frame ExposomeSet
# @aliases as.data.frame,ExposomeSet-methods
# @export as.data.frame.ExposomeSet
# as.data.frame.ExposomeSet <- function(x, row.names = NULL, optional = FALSE, phe = TRUE, ...) {
#     mtrc <- data.frame(t(assayDataElement(x, "exp")))
#
#     for(ex in colnames(mtrc)) {
#         if(fData(x)[ex, "_type"] == "factor") {
#             mtrc[ , ex] <- as.factor(mtrc[ , ex])
#         } else {
#             mtrc[ , ex] <- as.numeric(as.character(mtrc[ , ex]))
#         }
#     }
#     if(phe) {
#         phn <- pData(x)
#         cbind(mtrc, phn)
#     } else {
#         mtrc
#     }
# }
