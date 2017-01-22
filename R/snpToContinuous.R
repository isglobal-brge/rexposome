#' Transforms the discrete genotype from a \code{snpSet} to a \code{matrix} of 
#' a continuous variable.
#'
#' The function converts the categorical variable of SNPs to a continuous 
#' variable by normalizing each SNP as described in Abraham G. and Inouye M. 
#' 2014 (DOI: 10.1371/journal.pone.0093766).
#'
#' @rdname snpToContinuous
#' @aliases snpToContinuous,SnpSet-method
#' @param snpSet An object of class \code{snpSet} with set calls slot .
#' @param verbose If set to \code{TRUE}, messages will be shown.
#' @return An \code{matrix} of the calls of the SNPs converted to a continuous
#' variable.
#' @export snpToContinuous
#' @seealso \link{crossomics} use this function when a 
#' \link{MultiDataSet} si given with a dataset of SNPs
snpToContinuous <- function(snpSet, verbose = FALSE) {
  .snpToNumeric <- function(x) {
    xx <- as.numeric(x) - 1
    xx[xx<0] <- NA
    return(xx)
  }

  .normSNP <- function(x) {
    mu <- mean(x)
    p <- mu/2
    ans <- (x - mu) / sqrt(p*(1-p))
    ans
  }
  if(class(snpSet) != "SnpSet") { stop("Argumpent 'snpSet' must be an object of class 'SnpSet'.") }

  if(verbose) { message("Getting calls from SNP") }
  cc <- snpCall(snpSet)
  
  if(verbose) { message("Transforming genotype to numeric") }
  snpN <- do.call(cbind,
    mclapply(1:ncol(cc), function(nc){ .snpToNumeric(cc[ , nc]) })
  )
  rm(cc)
  if(verbose){ message("Transforming numeric to frequency")}
  snpN <- do.call(rbind, 
    mclapply(1:nrow(snpN), function(nr){ .normSNP(snpN[nr, ]) })
  )
}

