% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposome.R
\docType{data}
\name{expo}
\alias{expo}
\title{\code{ExposomeSet} for testing purpouses}
\format{
An object of class \code{ExposomeSet} of dimension 104 x 1200 x 4.
}
\usage{
data("exposome")
}
\value{
An \code{ExposomeSet} object.
}
\description{
\code{ExposomeSet} with full set of 104 exposures,
 1200 simulated samples and 4 phenotypes (asthma status,
 BMI measure, sex, and age).
}
\examples{
data("exposome")
dim(expo)
exposureNames(expo)
familyNames(expo)
sampleNames(expo)
phenotypeNames(expo)
}
\keyword{datasets}
