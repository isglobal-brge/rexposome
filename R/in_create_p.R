.create_p <- function(expo.dt, omic.p, select) {
  dt <- data.frame(n = 1:nrow(expo.dt))
  for(pe in select) {
    found <- FALSE
    if(!found & ncol(expo.dt) > 1) {
      if(pe %in% colnames(expo.dt)) {
        dt <- cbind(dt, expo.dt[ , pe])
        found <- TRUE
      }
    }
    if(!found & ncol(omic.p) > 1) {
      if(pe %in% colnames(omic.p)) {
        dt <- cbind(dt, omic.p[ , pe])
        found <- TRUE
      }
    }
    if(!found) {
      stop("'", pe, "' in design not found.")
    }
  }
  dt <- dt[, 2:ncol(dt), drop=FALSE]
  colnames(dt) <- select
  return(dt)
}


## old version
# .create_p <- function(expo.e, expo.p, omic.p, select) {
#     dt <- data.frame(n = 1:nrow(expo.p))
#     for(pe in select) {
#         found <- FALSE
#         if(!found & ncol(expo.e) > 1) {
#             if(pe %in% colnames(expo.e)) {
#                 dt <- cbind(dt, expo.e[ , pe])
#                 found <- TRUE
#             }
#         }
#         if(!found & ncol(expo.p) > 1) {
#             if(pe %in% colnames(expo.p)) {
#                 dt <- cbind(dt, expo.p[ , pe])
#                 found <- TRUE
#             }
#         }
#         if(!found & ncol(omic.p) > 1) {
#             if(pe %in% colnames(omic.p)) {
#                 dt <- cbind(dt, omic.p[ , pe])
#                 found <- TRUE
#             }
#         }
#         if(!found) {
#             stop("'", pe, "' in design not found.")
#         }
#     }
#     dt <- dt[, 2:ncol(dt), drop=FALSE]
#     colnames(dt) <- select
#     return(dt)
# }

