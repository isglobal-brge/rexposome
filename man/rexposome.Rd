% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rexposome.R
\docType{package}
\name{rexposome}
\alias{rexposome}
\title{rexposome: Package for exposome exploration and outcome data analysis}
\description{
#' @section exposures loading and exposures managment:
\code{rexposome} offers two methods to bring exposome data to R and
Bioconductor. \code{\link{readExposome}} allows to read three txt-like
files (\code{.csv}, \code{.tsv}, ...) while \code{\link{loadExposome}}
is sued with \code{matrix} and \code{data.frame}s. The class obtained is
an \code{\link{ExposomeSet}}, a class based in \code{eSet} for exposome
data managment.
}
\section{exposures processing}{

The packages offers a wide set of functions to preprocess exposome data.
Method \code{\link{trans}} allow to transforms the exposures, method
\code{\link{normalityTest}} allows to check for normality in exposome,
\code{\link{standardize}} allows to standardize the exposures, among others.
Finally, \code{\link{impute}} and \code{\link{ilod}} allow tu use \code{mice},
\code{Hmisc} and \code{imputeLCMD} for exposure missing data and exposure
under-lod data imputation.
}

\section{exposures analyses}{

the two methods \code{\link{exwas}} and \code{\link{mexwas}} allows to test
the association between exposures and health outcomes (phenotpe data).
}

\section{exposures plotting}{

The methods \code{\link{plotFamily}} allows to see how the exposures
behaves within families. \code{\link{plotCorrelation}} helps to understand
how exposures are related between themselves. \code{\link{plotClassification}}
allos to visually detect cuslters of samples that share the same pattern
of levels of exposures.
}

