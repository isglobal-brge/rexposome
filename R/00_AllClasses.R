#' Class ExposomeSet
#'
#' Class \code{ExposomeSet} contains the exposure levels, the exposure's
#' description and the samples phenotype. It is the starting object for
#' \code{rexposome} package and extends \link{eSet}.
#'
#' @name ExposomeSet
#' @aliases ExposomeSet-class
#' @rdname ExposomeSet-class
#' @exportClass ExposomeSet
#' @slot assayData Contains the exposures matrix with column number equal to
#' \code{nrow(phenoData)} (see \link{eSet}, \link{AssayData}).
#' @slot featureData Contains the description of the exposures including
#' the family where they belong (see \link{eSet}, \link{AnnotatedDataFrame}).
#' @slot phenoData: Contains the phenotypes or variables experimenter-supplied
#' (see \link{eSet}, \link{AnnotatedDataFrame}).
#' @slot experimentData NOT USED
#' @slot annotation NOT USED
#' @seealso \code{\link{read_exposome}} to create an \code{ExposomeSet}
#' from files, \code{\link{load_exposome}} to create an \code{ExposomeSet}
#' from \code{data.frames}
#' @return An object of class \code{ExposomeSet}
setClass(
  Class = "ExposomeSet",
  contains = "eSet",
  prototype = prototype(
    new("VersionedBiobase",
        versions = c(classVersion("eSet"), ExposomeSet = "1.0.0")
    )
  )
)

#' Class ExposomeCorr
#'
#' Class \code{ExposomeCorr} contains a matrix of correlations between
#' continuos exposures calculated using \link{cor}. It also contains the
#' description of each exposures (\code{fData} of the original
#' \link{ExposomeSet}) in order to maintain a coherence
#' with the original source. It extends \link{eSet-class}.
#'
#' @name ExposomeCorr
#' @aliases ExposomeCorr-class
#' @rdname ExposomeCorr-class
#' @exportClass ExposomeCorr
#' @slot assayData Contains the correlation matrix
#' (see \link{eSet}, \link{AssayData}).
#' @slot featureData Contains the description of the exposures including
#' the family where they belong (see \link{eSet}, \link{AnnotatedDataFrame}).
#' @seealso \code{\link{pca}} to study the behavioud between samples
#' and exposures in an \code{\link{ExposomeSet}}
setClass(
  Class = "ExposomeCorr",
  contains = "eSet"
)

#' Class ExposomePCA
#'
#' Class \code{ExposomePCA} contains a matrix of exposures used to
#' compute the PCA, also a table of phenotypes and a set congtaing the
#' multiple results of computing the PCA.
#'
#' @name ExposomePCA
#' @aliases ExposomePCA-class
#' @rdname ExposomePCA-class
#' @exportClass ExposomePCA
#' @slot pca list containing all elements of the PCA
#' @slot phenoData Contains the phenotypes or variables experimenter-supplied
#' (see \link{eSet}, \link{AnnotatedDataFrame}).
#' @slot featureData Contains the description of the exposures including
#' the family where they belong (see \link{eSet}, \link{AnnotatedDataFrame}).
#' @seealso \code{\link{correlation}} to study the correlation between
#' exposures in a \code{\link{ExposomeSet}}
#' @return An object of class \code{ExposomePCA}
setClass(
    Class = "ExposomePCA",
    contains = "eSet",
    representation = representation(
        pca = "list",
        phenoData = "AnnotatedDataFrame",
        featureData = "AnnotatedDataFrame"
    )
)


#' Class ExWAS
#'
#' Class \code{ExWAS} obtained from \link{exwas} method of an
#' \link{ExposomeSet} object, contains the result of testing the association
#' of exposures of an \code{ExposomeSet} to its phenotypes. "ExWAS" is the
#' acronym of "Exposome-Wide Association  Study". The function can be applied
#' to one of to many phenotypes in the \code{ExposomeSet} object.
#'
#' @name ExWAS
#' @aliases ExWAS-class
#' @rdname ExWAS-class
#' @exportClass ExWAS
#' @slot effective Number containing the effective number of tests.
#' @slot formula Tested formula.
#' @slot comparison Result of performing the test to find association between
#' levels of exposures and phenotype.
#' @slot description Description of the exposures used in the ExWAS.
#' (in description file).
#' @seealso \code{\link{exwas}} to perform an Exposome-Wide  Association Study
#' and to create an \code{\link{ExWAS}}, \code{\link{mexwas}} to perform a
#' Multivariate Exposome-Wide Association Study and to create a
#' \code{\link{mExWAS}}
#' @return An object of class \code{ExWAS}
setClass(
  Class = "ExWAS",
  representation = representation(
    effective = "numeric",
    formula = "formula",
    comparison = "data.frame",
    description = "data.frame"
  )
)

#' Class nlExWAS
#'
#' Class \code{nlExWAS} obtained from \link{nl_exwas} method of an
#' \link{ExposomeSet} object, contains the result of testing the multiple
#' models of exposures of an \code{ExposomeSet} to a set of given phenotypes.
#' "nlExWAS" is the #' acronym of "Multivariate Exposome-Wide Association
#' Study".
#'
#' @name nlExWAS
#' @aliases nlExWAS-class
#' @rdname nlExWAS-class
#' @exportClass nlExWAS
#' @slot ranking Results obtained from the \code{mboost} package as
#' \code{data.frame}.
#' @slot models Models tested in the \code{mboost} package.
#' @slot n Number of samples (exposures) used.
#' @slot predictors Predictors obained from \code{mboost} package.
#' @slot X \code{data.fame} with the phenotypes used in the models.
#' @slot xyformula Formulas used in \code{mboost} package.
#' @return An object of class \code{nlExWAS}
setClass(
    Class = "nlExWAS",
    representation = representation(
        ranking = "data.frame",
        models = "list",
        n = "numeric",
        predictors = "list",
        X = "data.frame",
        xyformula = "list"
    )
)

#' Class mExWAS
#'
#' Class \code{mExWAS} obtained from \link{mexwas} method of an
#' \link{ExposomeSet} object, contains the result of testing the multiple
#' models of exposures of an \code{ExposomeSet} to a set of given phenotypes.
#' "mExWAS" is the #' acronym of "Multivariate Exposome-Wide Association
#' Study".
#'
#' @name mExWAS
#' @aliases mExWAS-class
#' @rdname mExWAS-class
#' @exportClass mExWAS
#' @slot result klist with the fitted model and result.
#' @slot phenotype Name of the phenotype used in the analysys.
#' @slot description feature data from original ExposomeSet.
#' @seealso \code{\link{mexwas}} to perform a Multivariate Exposome-Wide
#' Association Study and to create a \code{\link{mExWAS}},
#' \code{\link{exwas}} to perform an Exposome-Wide  Association Study
#' and to create an \code{\link{ExWAS}}
#' @return An object of class \code{mExWAS}
setClass(
    Class = "mExWAS",
    representation = representation(
        result = "list", # fit
        phenotype = "character",
        description = "data.frame"
    )
)

#' Class ExposomeClust
#'
#' Class \code{ExposomeClust} obtained from \link{clustering} on an
#' \link{ExposomeSet} object, represents the groups of samples created
#' applying a clustering method on the \code{ExposomeSet}' exposures.
#'
#' @name ExposomeClust
#' @aliases ExposomeClust-class
#' @rdname ExposomeClust-class
#' @exportClass ExposomeClust
#' @slot model Result obtained on applying \code{method} on the exposures.
#' @slot method Function used to perform the clustering of the exposures.
#' @slot call Call used to create this object.
#' @slot samples Name of the exposures after the clustering process.
#' @seealso \code{\link{clustering}} to apply a clustering on an
#' \code{\link{ExposomeSet}} and create an \code{\link{ExposomeClust}}.
#' @return An object of class \code{ExposomeClust}
setClass(
  Class = "ExposomeClust",
  contains = "ExposomeSet",
  representation = representation(
    model = "list",
    method = "character",
    call = "character",
    samples = "character"
  )
)
