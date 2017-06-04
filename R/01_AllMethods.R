#' Getter to obtain the exposures's names of an ExposomeSet or ExposomePCA
#'
#' @name exposureNames
#' @rdname exposureNames-methods
#' @aliases exposureNames
#' @param object \link{ExposomeSet} that will be queried for the exposures's
#' names.
#' @return The name of the exposures as a character vector.
#' @examples
#' data("exposome")
#' exposureNames(expo)
#' @export exposureNames
#' @seealso \link{phenotypeNames} to get the phenotypes,
#' \link{familyNames} to get the families of exposures
#' @section Warning:
#' \link{exposureNames} collides with \link{featureNames} of \link{eSet}.
#' Although in \code{rexposome 1.0.0} both function can be used as
#' synonyms, this usage is discouraged and it is not assured.
setGeneric("exposureNames", function(object)
    standardGeneric("exposureNames")
)

#' Getter to obtain the phenotype's names of an ExposomeSet or ExposomePCA.
#'
#' @name phenotypeNames
#' @rdname phenotypeNames-methods
#' @aliases phenotypeNames
#' @param object \code{ExposomeSet} that will be queried for the phenotype's
#' names.
#' @return The name of the phenotypes as a a character vector.
#' @examples
#' data("exposome")
#' phenotypeNames(expo)
#' @export phenotypeNames
#' @seealso \link{exposureNames} to get the name of the exposures,
#' \link{familyNames} to get the families of exposures
setGeneric("phenotypeNames", function(object)
    standardGeneric("phenotypeNames")
)

#' Getter to obtain the exposures's names of an ExposomeSet.
#'
#' This method returns the name of the families in an \link{ExposomeSet}, but
#' it can return a vector, labeled with the exposures in the
#' \link{ExposomeSet}, containing the family belonging to each exposure.
#'
#' @name familyNames
#' @rdname familyNames-methods
#' @aliases familyNames
#' @param object \code{ExposomeSet} that will be queried for the exposures's
#' family-names.
#' @param by.exposure (default \code{FALSE}) If \code{TRUE} a vector
#' labeled with each exposure name will be returned with the family of each
#' exposures. If \code{FALSE} a vector with the (unique) name of the families
#' of exposures will be returned.
#' @return The families of the exposures into the \link{ExposomeSet}, or the
#' family of each exposure into the \link{ExposomeSet}.
#' @examples
#' data("exposome")
#' # Get families
#' familyNames(expo)
#' # Get the family of each exposure
#' familyNames(expo, by.exposure = TRUE)
#' @export familyNames
#' @seealso \link{exposureNames} to get the name of the exposures,
#' \link{phenotypeNames} to get the phenotypes
setGeneric("familyNames", function(object, by.exposure = FALSE)
    standardGeneric("familyNames")
)

#' Returns the exposures matrix of an ExposomeSet.
#'
#' Given an \link{ExposomeSet} it returns the inner matrix of exposures,
#' having the exposures as columns and the samples as rows.
#'
#' @name expos
#' @rdname expos-methods
#' @aliases expos
#' @param object An \link{ExposomeSet}.
#' @return A matrix of exposures
#' @examples
#' data("exposome")
#' expos(expo)[1:3, 1:3]
#' @export expos
setGeneric("expos", function(object)
    standardGeneric("expos")
)


#' #' Summary of an ExposomeSet.
#' #'
#' #' Given an \link{ExposomeSet} is shows a summary for its exposures or
#' #' its phenotypes.
#' #'
#' #' @name Summary
#' #' @rdname Summary-methods
#' #' @aliases Summary
#' #' @param object code{ExposomeSet} with 'set' will be summarized.
#' #' @param set Set to be sumarized (\code{"exposures"} or \code{"phenotypes"}).
#' #' @param select Subseting of exposures of phenotypes.
#' #' @return A basic description of the exposures in the \code{ExposomeSet}
#' #' @examples
#' #' data("exposome")
#' #' Summary(expo, set = "exposures")
#' #' @export Summary
#' setGeneric("Summary", function(object, set=c("exposures", "phenotypes"),
#'                                select)
#'     standardGeneric("Summary")
#' )


#' Restore original data of an ExposomeSet.
#'
#' Given an \link{ExposomeSet} is restores the original data and remove
#' all changes (standardization, transformation, imputation) that exposures
#' could have.
#'
#' @name restore
#' @rdname restore-methods
#' @aliases restore
#' @param object On object of class \code{ExposomeSet}.
#' @return An \code{ExposomeSet} with no transformations.
#' @examples
#' data("exposome")
#' restore(expo)
#' @export restore
setGeneric("restore", function(object)
    standardGeneric("restore")
)

# -----------------------------------------------------------------------------

#' Standardize of an ExposomeSet.
#'
#' Given an \link{ExposomeSet} it standardizes the exposures by using mean/sd
#' if method is \code{"normal"} or by using median/mad if method is
#' \code{"robust"}.
#'
#' @name standardize
#' @rdname standardize-methods
#' @aliases standardize
#' @param object \code{ExposomeSet} with exposures to be standardized.
#' @param select Subseting of exposures of phenotypes.
#' @param method (default \code{"normal"}) Character selecting the method to be
#' applied (\code{"normal"}
#' \code{"iqr"} or \code{"robust"}).
#' @param na.rm (by default \code{TRUE}) Removes NA data to perform
#' standardization.
#' @param warnings (defaulr \code{TRUE}) If set to \code{FALSE} warnings are
#' not shown.
#' @return An \code{ExposomeSet} with the selected exposures standardized and
#' keeping the others exposures as the original input object.
#' @examples
#' data("exposome")
#' exp.sn <- standardize(expo, method = "normal", select = "lbde100_lip")
#' exp.rs <- standardize(expo, method = "iqr", select = "lbde100_lip")
#' exp.rs <- standardize(expo, method = "robust", select = "lbde100_lip")
#' @export standardize
#' @seealso \link{highAndLow} to transform the continuous exposures to
#' levelled factors, \link{trans} to transform the exposures
setGeneric("standardize", function(object, select, method = "normal",
                                   na.rm = TRUE, warnings = TRUE)
    standardGeneric("standardize")
)

#' Function to apply a transformation to the exposures of an ExposomeSet
#'
#' The exposures in an \link{ExposomeSet} can be transformed using this
#' function. \code{transform} apples a function \code{fun} to the selected
#' exposures.
#'
#' @name trans
#' @rdname trans-methods
#' @aliases trans
#' @param object \code{ExposomeSet} which exposures will be transformed.
#' @param fun Function to be applied on the exposures.
#' @param select If not set, receive the name of all exposures. It can takes a
#' character vector to select specific exposures.
#' @param by.exposure (default \code{FALSE}) If \code{TRUE} applies \code{fun}
#' to each exposure (given as a vector ). If \code{FALSE} the matrix of
#' exposures is fiven to \code{fun}.
#' @param ... Argument given to \code{fun}.
#' @return A new \code{ExposomeSet} with selected exposures transformed with
#' \code{fun}.
#' @examples
#' data("exposome")
#' exp.t <- trans(expo, fun = log, select = "ldde_lip")
#' @export trans
#' @seealso \link{highAndLow} to transform the continuous exposures to levelled
#' factors, \link{standardize} to standardize by normal or robust methods the
#' exposures
setGeneric("trans", function(object, fun, select, by.exposure = FALSE, ...)
    standardGeneric("trans")
)

#' Function to convert continuous exposures to categorical exposures
#'
#' This method allows to convert continuous exposures of an \code{ExposomeSet}
#' to categorical exposures using the n-percentile groups, defined by
#' \code{ngroups} argument. By default, all levels are kept but if
#' \code{intervals} is set to \code{"extrem"}, the levels between the extrems
#' (aka. lowes and highest) are discarted and their values set to \code{NA}.
#'
#' @name highAndLow
#' @rdname highAndLow-methods
#' @aliases highAndLow
#' @param object An object of class \code{ExposomeSet}.
#' @param ngroups (default \code{3}) Number of intervals to be created.
#' @param intervals (default \code{"standard"}) If set to \code{"sctandard"}
#' all levels are set. If set to \code{"extreme"} obly lowes and highest levels
#' are kept (others are set to \code{NA}).
#' @param select (optional) Subset of exposures where the discretization is
#' applied. If missing, all exposures are used.
#' @param drop (default \code{FALSE}) If set to \code{FALSE} original exposures
#' are kept and discretized exposures are add to \code{ExposomeSet}. If set to
#' \code{TRUE}, original exposures are replaced by categorical exposures.
#' @param warnings (defaulr \code{TRUE}) If set to \code{FALSE} warnings are
#' not shown.
#' @return A new \code{ExposomeSet} with categorical exposures.
#' @examples
#' # No drop
#' data("exposome")
#' exp.hl <- highAndLow(expo, intervals = "standard", select = "ldde_lip")
#' dim(exp.hl)
#' # exposures   samples phenotyes
#' #       105      1200         4
#' dim(expo)
#' # exposures   samples phenotyes
#' #       104      1200         4
#' # exps.hl has 107 exposures: the original 104 plus the new
#' #         3 factored exposures
#'
#' # Drop
#' exp.hl <- highAndLow(expo, intervals = "standard",
#'     select = "ldde_lip", drop = TRUE)
#' dim(exp.hl)
#' # exposures   samples phenotyes
#' #       104      1200         4
#' dim(expo)
#' # exposures   samples phenotyes
#' #       104      1200         4
#' @export highAndLow
#' @seealso \code{\link{trans}} to transform exposures,
#' \code{\link{standardize}} to standardize exposures.
setGeneric("highAndLow", function(object, ngroups = 3,
        intervals = "standard", select, drop = FALSE, warnings = TRUE)
    standardGeneric("highAndLow")
)

# -----------------------------------------------------------------------------

#' It creates a \code{data.frame} of boolean indicating if the exposures follows
#' a normal distribution or not.
#'
#' This functions uses \code{\link{shapiro.test}} to test the normality of the
#' exposures and returns a \code{data.frame} with a boolean value and a
#' p-value for each exposure.
#'
#' @name normalityTest
#' @rdname normalityTest-methods
#' @aliases normalityTest
#' @param object \link{ExposomeSet} with the exposome to be tested.
#' @param exposure Name of the exposure to be tested, if missing all the
#' exposures will be tested.
#' @param th (by default \code{0.05}) Threshold to considere an exposure to
#' follow a normal distribution.
#' @param min.val (by default \code{5}) Minimum number of values not missings
#' to test the exposures.
#' @param na.rm (by default \code{TRUE}) Removes the NA values to test the
#' normality on the exposure.
#' @return A \code{data.frame} with three columns: exposure, normality and
#' p.value. "exposure" column contains the name of each exposure. "normalty"
#' column contains a \code{logical} value indicating if the obtained p-value
#' is under the given threshold. "p.value" column contains the obtained p-value
#' from \code{\link{shapiro.test}}.
#' @examples
#' data("exposome")
#' normalityTest(expo)
#' @export normalityTest
#' @seealso \link{plotHistogram} to draw the shape of an exposure,
#' \link{plotMissings} to draw a plot with the missing data an ExposomeSet,
#' \link{imputation} to impute missing data on the exposures of an ExposomeSet
setGeneric("normalityTest", function(object, exposure, th = 0.05, min.val = 5,
                                     na.rm = TRUE) {
    standardGeneric("normalityTest")
})

#' It creates a vector with the amount of missing in an ExposomeSet
#'
#' This function can be used to obtain a table with the missing data in
#' exposures and in phenotypes of an \link{ExposomeSet}.
#'
#' @name tableMissings
#' @rdname tableMissings-methods
#' @aliases tableMissings
#' @param object \link{ExposomeSet} which exposome will be plotted.
#' @param set Can be set to \code{"exposures"} or to \code{"phenotypes"}.
#' @param output (default \code{"n"}) Can be \code{"n"} for number of values,
#' and \code{"p"} for percentage.
#' @param sort (default \code{TRUE}) If \code{TRUE} the chart will be ordered
#' from the features with less missing data to the ones with more missing data.
#' @return A numeric vector with number of missing values per exposure. The
#' vector is labeled with the exposure's names.
#' @examples
#' data("exposome")
#' # The included has no missing data
#' tableMissings(expo, set = "exposures")
#' tableMissings(expo, set = "phenotypes")
#' @export tableMissings
#' @seealso \link{plotFamily} to draw the profile of a family of exposures,
#' \link{plotHistogram} to draw the shape of an exposure,
#' \link{plotMissings} to draw a plot with the missing data an ExposomeSet,
#' \link{imputation} to impute missing data on the exposures of an ExposomeSet
setGeneric("tableMissings", function(object, set, output = "n", sort = TRUE) {
    standardGeneric("tableMissings")
})

#' It creates a vector with the amount of under-LOD exposures in an ExposomeSet
#'
#' This function can be used to obtain a table with the under-LOD data in
#' exposures of an \link{ExposomeSet}.
#'
#' @name tableLOD
#' @rdname tableLOD-methods
#' @aliases tableLOD
#' @param object \link{ExposomeSet} which exposome will be plotted.
#' @param output (default \code{"n"}) Can be \code{"n"} for number of values,
#' and \code{"p"} for percentage.
#' @param lod.col (default \code{"LOD"}) Name of the column in \code{fData}
#' containing the LOD thresholds.
#' @param sort (default \code{TRUE}) If \code{TRUE} the chart will be ordered
#' from the features with less missing data to the ones with more under-LOD
#' values.
#' @return A numeric vector with number of udner-LOD values per exposure. The
#' vector is labeled with the exposure's names.
#' @examples
#' data("exposome")
#' # The included has no missing data
#' tableLOD(expo, output = "n")
#' @export tableLOD
#' @seealso \link{plotFamily} to draw the profile of a family of exposures,
#' \link{plotHistogram} to draw the shape of an exposure,
#' \link{plotMissings} to draw a plot with the missing data an ExposomeSet,
#' \link{imputation} to impute missing data on the exposures of an ExposomeSet
setGeneric("tableLOD", function(object, output = "n", lod.col = "LOD",
                                sort = TRUE) {
    standardGeneric("tableLOD")
})

# -----------------------------------------------------------------------------

#' It draws a chart with the percentage of missing in an ExposomeSet
#'
#' This function can be used to draw the number of missing data in exposures and
#' in phenotypes of an \link{ExposomeSet}.
#'
#' @name plotMissings
#' @rdname plotMissings-methods
#' @aliases plotMissings
#' @param object \link{ExposomeSet} which exposome will be plotted.
#' @param set Can be set to \code{"exposures"} or to \code{"phenotypes"}.
#' @param x.max (default \code{100}) Fix the maxium value of the X-axis.
#' @param sort (default \code{TRUE}) If \code{TRUE} the chart will be ordered
#' from the features with less missing data to the ones with more missing data.
#' @return A \code{ggplot} object.
#' @examples
#' data("exposome")
#' # The included has no missing data
#' plotMissings(expo, set = "exposures")
#' plotMissings(expo, set = "phenotypes")
#' @export plotMissings
#' @seealso \link{plotFamily} to draw the profile of a family of exposures,
#' \link{plotHistogram} to draw the shape of an exposure,
#' \link{tableMissings} to get a table with the missing data of an ExposomeSet,
#' \link{imputation} to impute missing data on the exposures of an ExposomeSet
setGeneric("plotMissings", function(object, set, x.max = 100, sort = TRUE)
    standardGeneric("plotMissings")
)

#' It draws a chart with the percentage of under-LOD values in an ExposomeSet
#'
#' This function can be used to draw the amount of under-LOD values in the
#' exposures of an \link{ExposomeSet}.
#'
#' @name plotLOD
#' @rdname plotLOD-methods
#' @aliases plotLOD
#' @param object \link{ExposomeSet} which exposome will be plotted.
#' @param lod.col (default \code{"LOD"}) Name of the column in \code{fData}
#' containing the LOD thresholds.
#' @param x.max (default \code{100}) Fix the maxium value of the X-axis.
#' @param sort (default \code{TRUE}) If \code{TRUE} the chart will be ordered
#' from the features with less missing data to the ones with more under-LOD
#' values.
#' @return A \code{ggplot} object.
#' @examples
#' data("exposome")
#' # The included has no missing data
#' plotLOD(expo)
#' @export plotLOD
#' @seealso \link{plotFamily} to draw the profile of a family of exposures,
#' \link{plotHistogram} to draw the shape of an exposure,
#' \link{tableMissings} to get a table with the missing data of an ExposomeSet,
#' \link{imputation} to impute missing data on the exposures of an ExposomeSet
setGeneric("plotLOD", function(object, lod.col = "LOD", x.max = 100,
                               sort = TRUE)
    standardGeneric("plotLOD")
)

#' It draws the profile of the exposome in an ExposomeSet
#'
#' This function draw a profile of the full exposome into an \link{ExposomeSet}
#' or the profile of the exposures in a specific family. For continuous
#' families, box-plots are drawn; while  for categorical families accumulative
#' bar-charts.
#'
#' @name plotFamily
#' @rdname plotFamily-methods
#' @aliases plotFamily
#' @param x \link{ExposomeSet} which exposome will be plotted.
#' @param family Name of the familty that will be drawn. \code{'all'} is
#' allowed to draw a grid with all the families.
#' @param group If set it displays the family grouped
#' by the given phenotype.
#' @param group2 If set it displays the family grouped
#' by the given phenotype.
#' @param scatter (default \code{TRUE}) If the family to be plotted is
#' continuous, the samples will be shown.
#' @param na.omit (default \code{TRUE}) Do not show \code{NA} values.
#' @return A \code{ggplot} object if a family was selected. \code{invisible()}
#' if argument \code{family} was set to \code{"all"}.
#' @examples
#' data("exposome")
#' plt <- plotFamily(expo, family = "Metals")
#' plt <- plt + ggplot2::ggtitle("Metals")
#' plt
#' plt <- plotFamily(expo, family = "Indoor air")
#' plt <- plt + ggplot2::ggtitle("Indoor air")
#' plt
#' @export plotFamily
#' @seealso \link{plotHistogram} to draw the shape of an exposure,
#' \link{plotMissings} to plot the missing data from an \link{ExposomeSet}
setGeneric("plotFamily", function(x, family, group, group2, scatter = TRUE,
                                  na.omit=TRUE)
    standardGeneric("plotFamily")
)

#' It draws a histogram for each exposure in an ExposomeSet
#'
#' It draws a grid with an histogram per exposure in an \link{ExposomeSet}.
#'
#' @name plotHistogram
#' @rdname plotHistogram-methods
#' @aliases plotHistogram
#' @param x \link{ExposomeSet} which exposome will be plotted.
#' @param select Name fo the exposure to be plotted. If missing, all
#' exposures will be used.
#' @param density (default \code{TRUE}) If \code{TRUE} a density plot is
#' drawn overlapping the histogram.
#' @param show.trans (default \code{FALSE}) If set to \code{TRUE} it will draw
#' a panel of auxiliar plots with the continuous exposure transformed with
#' \code{log} and \code{sqrt}.
#' @return A \code{ggplot} object.
#' @examples
#' data("exposome")
#' plotHistogram(expo[1:3, ], select = "ldde_lip")
#' @export plotHistogram
#' @seealso \link{plotFamily} to draw the profile of a family of exposures,
#' \link{plotMissings} to plot the missing data from an \link{ExposomeSet}
setGeneric("plotHistogram", function(x, select, density = TRUE,
                                     show.trans = FALSE)
    standardGeneric("plotHistogram")
)

# -----------------------------------------------------------------------------

#' Function to impute missing values from an ExposomeSet
#'
#' This function is a wrapper of the functions \code{mice} and \code{complete}
#' from the package \code{mice}. Also to the \code{impute} from the
#' package \code{Hmisc}. The function is designed to use those functions
#' to impute missing values on exposures (not in phenotypes).
#'
#' @name imputation
#' @rdname imputation-methods
#' @aliases imputation
#' @param object \code{ExposomeSet} which exposures will be imputed.
#' @param select Exposures to be imputed. If missing, all exposes will be
#' imputed.
# @param ssystem (default \code{"mice"}) Argument to select the imputation
# package. Can take values \code{"mice"} or \code{"hmisc"}
#' @param messages (default \code{FALSE}) If set to \code{TRUE} messages from
#' \code{mice}'s function will be displayed.
#' @param ... Argument given to function \code{mice} of pakcage \code{mice}
#' (\code{printFlag} is set from \code{messages}).
#' @return A new \code{ExposomeSet} with the imputed exposures.
#' @examples
#' \dontrun{
#' #Being x an ExposomeSet
#' x <- imputation(x)
#' }
#' @export imputation
#' @seealso \link{plotMissings} to plot the missing data of an ExposomeSet,
#' \link{tableMissings} to get a table with the missing data of an ExposomeSet
setGeneric("imputation", function(object, select, ..., messages=FALSE)
    standardGeneric("imputation")
)

#' Function to impute under-LOD values from an ExposomeSet
#'
#' This function is a wrapper of the functions \code{impute.MinProb}
#' from the package \code{imputeLCMD}.
#'
#' @name ilod
#' @rdname ilod-methods
#' @aliases ilod
#' @param object \code{ExposomeSet} which exposures will be imputed.
#' @param seed (default \code{1234}) Seed to make the imputation reproducible.
#' @param lod.col (default \code{"LOD"}) Name of the column in \code{fData}
#' with the threshold of the LOD.
#' @param pNA (default \code{0.2}) Maximum percentage allowed of values under
#' LOD
#' @param tLog (default \code{FALSE}) If set to \code{TRUE} it transforms all
#' the exposures to lod before the imputation.
#' @param method (default \code{"QRILC"}) Method to be used to impute the
#' under-LOD values. Two allowed: QRILC method (value \code{"QRILC"}) and
#' stochastic minimal value approach (value \code{"MinProb"}).
#' @param warnings (default \code{TRUE}) If set to \code{FALSE} warnings will
#' not be displayed.
#' @param ... Arguments passed to \code{impute.QRILC} or \link{impute.MinProb}
#' from \code{imputeLCMD}.
#' @return A new \code{ExposomeSet} with the imputed exposures.
#' @examples
#' \dontrun{
#' #Being x an ExposomeSet
#' x <- ilod(x)
#' }
#' @export ilod
#' @seealso \link{plotMissings} to plot the missing data of an ExposomeSet,
#' \link{tableMissings} to get a table with the missing data of an ExposomeSet
setGeneric("ilod", function(object, seed = 1234, lod.col = "LOD", pNA = 0.2,
                            tLog = FALSE, method = "QRILC", warnings = TRUE,
                            ...)
    standardGeneric("ilod")
)

# -----------------------------------------------------------------------------

#' Creation of an ExposomePCA from an ExposomeSet.
#'
#' Method to calculate a PCA based on the exposures of an \link{ExposomeSet}.
#' Only numerical-exposures (non categorical) will be computed. The
#' function resurns an \link{ExposomePCA} object. This PCA is performed
#' by using \link{FactoMineR} package.
#'
#' @name pca
#' @rdname pca-methods
#' @aliases pca
#' @param object \code{ExposomeSet} which exposures will be used for the PCA
#' @param npc (by default 10) number of dimensions kept in the results
#' @return An \code{ExposomePCA} with the values of the PCA.
#' @seealso \link{plotPCA} to plot the PCA values of an
#' \link{ExposomePCA}, \link{clustering} to see how the exposures can
#' cluster samples, \link{correlation} to compute the correlation between
#' exposures
#' @examples
#' data("exposome")
#' epca <- pca(expo[12:20, ])
#' @export pca
setGeneric("pca", function(object, npc = 10)
    standardGeneric("pca")
)

#' Number of principal components in an ExposomePCA.
#'
#' @name ndim
#' @rdname ndim-methods
#' @aliases ndim
#' @param object \code{ExposomePCA} to obtain the number of components.
#' @return The number of components in the \code{ExposomePCA}.
#' @seealso \link{plotPCA} to plot the PCA values of an
#' \link{ExposomePCA}, \link{clustering} to see how the exposures can
#' cluster samples, \link{correlation} to compute the correlation between
#' exposures
#' @examples
#' data("exposome")
#' epca <- pca(expo[3:7, 1:100])
#' ndim(epca)
#' @export ndim
setGeneric("ndim", function(object)
    standardGeneric("ndim")
)

#' Plot association score between phentoypes and PCA
#'
#' Function used to plot the association between the phentoypes in an
#' \link{ExposomePCA} and the values for each component of the PCA in the
#' same \link{ExposomePCA}
#'
#' @name plotPHE
#' @rdname plotPHE-methods
#' @aliases plotPHE
#' @param object An object oc class \link{ExposomePCA}
#' @param phenotype (optional) to select a set of phenotypes to be ploted.
#' If not given all are used.
#' @param exp2fac (default, \code{5}) Threshold to considere a phentoype
#' categorical (less or equal to) or continuous (more than).
#' @return An object of class \code{ggplot}.
#' @seealso \link{pca} to compute PCA on an \link{ExposomeSet}, \link{plotEXP}
#' to plot the correlation between exposures ans PCA,
#' \link{ExposomePCA} as main class
#' @examples
#' data("exposome")
#' epca <- pca(expo[3:7, 1:100])
#' plotPHE(epca)
#' @export plotPHE
setGeneric("plotPHE", function(object, phenotype, exp2fac = 5)
    standardGeneric("plotPHE")
)

#' Plot correlation between exposures and PCA
#'
#' Function used to plot the correlation between the exposures in an
#' \link{ExposomePCA} and the values for each component of the PCA in the
#' same \link{ExposomePCA}
#'
#' @name plotEXP
#' @rdname plotEXP-methods
#' @aliases plotEXP
#' @param object An object of class \link{ExposomePCA}
#' @param exposure (optional) to select a set of exposures to be ploted.
#' If not given all are used.
#' @return An object of class \code{ggplot}.
#' @seealso \link{pca} to compute PCA on an \link{ExposomeSet}, \link{plotPHE}
#' to plot the P-Value of association between phenotypes ans PCA,
#' \link{ExposomePCA} as main class
#' @examples
#' data("exposome")
#' epca <- pca(expo[3:7, 1:100])
#' plotEXP(epca)
#' @export plotEXP
setGeneric("plotEXP", function(object, exposure)
    standardGeneric("plotEXP")
)

#' Ploting PCA
#'
#' Methdo to draw a plot for PCA contained in an \link{ExposomePCA}
#'
#'
#' @name plotPCA
#' @rdname plotPCA-methods
#' @aliases plotPCA
#' @param object An onbject of class \link{ExposomePCA}
#' @param set Group (\code{"all"}, \code{"samples"} or \code{"exposures"})
#' taht will be ploted.
#' @param cmpX (default: \code{1}) component to be placed at X axis
#' @param cmpY (default: \code{2}) component to be placed at Y axis
#' @param show.exposures (default: \code{FALSE}) If set to true, labels indicating
#' the exposures are shown.
#' @param show.samples (default: \code{FALSE}) If set to true, labels indicating
#' the samples are shown.
#' @param phenotype If \code{set} is set to \code{"samples"} can be used to
#' color samples by phenotype
#' @return An object of class \code{ggplot} or an object of class
#' \code{gtable} if argument \code{set} was set to \code{"all"}.
#' @seealso \link{pca} to compite PCA on an \link{ExposomeSet}, \link{plotPCA}
#' to plot the PCA, \link{ExposomePCA} as main class
#' @examples
#' data("exposome")
#' epca <- pca(expo[3:7, 1:100])
#' # A grid with exposures space, samples space and explained variance
#' plotPCA(epca, set = "all")
#' # Only exposures space
#' plotPCA(epca, set = "exposures") + ggplot2::theme(legend.position = "bottom")
#' # Only samples space
#' plotPCA(epca, set = "samples")
#' # Only samples space but coloured by phenotype
#' plotPCA(epca, set = "samples", phenotype = "sex") +
#' ggplot2::theme(legend.position = "bottom")
#' @export plotPCA
setGeneric("plotPCA", function(object, set, cmpX = 1, cmpY = 2,
                               show.exposures = FALSE, show.samples = FALSE,
                               phenotype)
    standardGeneric("plotPCA")
)

#' Ploting PCA in a 3D space
#'
#' Methdo to draw a plot for samples using three PC contained in an
#' \link{ExposomePCA}
#'
#' @name plot3PCA
#' @rdname plot3PCA-methods
#' @aliases plot3PCA
#' @param object An onbject of class \link{ExposomePCA}
#' @param cmpX Component to be placed at X axis
#' @param cmpY Component to be placed at Y axis
#' @param cmpZ Component to be placed at Z axis
#' @param phenotype Used to color samples by phentoype
#' @param main Title for the plot
#' @param angle (default \code{35}) angle between x and y axis.
#' @param pch (default \code{16}) plotting "character", i.e. symbol to use.
#' @param legend (default \code{TRUE}) If \code{TRUE} shows the legend.
#' @param plines (default \code{TRUE}) If \code{TRUE} it draws the lines from
#' each dot to the base plane.
#' @return A \code{list} with different graphics option from \code{scatterplot3d}.
#' @seealso \link{pca} to compite PCA on an \link{ExposomeSet}, \link{plotPCA}
#' to plot the PCA, \link{ExposomePCA} as main class
#' data("exposome")
#' epca <- pca(expo[3:7, 1:100])
#' plot3PCA(epca, cmpX = 1, cmpY = 2, cmpZ = 3, phenotype = "sex")
#' @export plot3PCA
setGeneric("plot3PCA", function(object, cmpX, cmpY, cmpZ, phenotype, main,
                                angle = 35, pch = 16, legend = TRUE,
                                plines = TRUE)
    standardGeneric("plot3PCA")
)

#' Creation of an ExposomeCorr from an ExposomeSet.
#'
#' Method to calculate the correlation between the exposures of an
#' \link{ExposomeSet}. Only numerical-exposures (non categorical) will
#' be computed. The function creates and returns an \link{ExposomeCorr}
#' object.
#'
#' @name correlation
#' @rdname correlation-methods
#' @aliases correlation
#' @param object \code{ExposomeSet} which exposures will be used to calculate
#' their correlation
# @param select Exposures used to compute correlation. If missing the
# correlation between all exposures will be calculated.
#' @param ... Other arguments passed to \link{cor}, \link{cramersV} or
#' to \link{lm}.
#' @param warnings (default \code{TRUE}) If set to \code{FALSE} warnings will
#' not be displayed.
#' @return \code{ExposomeCorr} with the correlation between the selected
#' exposures and their description
#' @examples
#' data("exposome")
#' expo.c <- correlation(expo)
#' expo.c
#' @export correlation
#' @seealso \link{plotCorrelation} to plot the correlations of an
#' \link{ExposomeCorr}, \link{clustering} to see how the exposures can
#' cluster samples, \link{pca} to compute PCA on exposures
setGeneric("correlation", function(object, ..., warnings = TRUE)
    standardGeneric("correlation")
)


#' It draws both circos or matrix plot for the correlation in ExposomeCorr
#'
#' While the circos plot can be used to see the general behaviours intra and
#' extra families of exposures, the matrix plot allows for a detailed view of
#' the correlations within an \code{ExposomeCorr} object.
#'
#' @name plotCorrelation
#' @rdname plotCorrelation-methods
#' @aliases plotCorrelation
#' @param object \code{ExposomeCorr} which correlations will be plotted.
#' @param type (default \code{"circos"}) Can take both \code{"circos"} or
#' \code{"matrix"}.
#' @param ... Arguments given to \code{corrplot} of package \link{corrplot}
#' if a matrix is draw. Moreover extra arguments are
#' can be passed to inner functions to draw both the matrix and the
#' circos of correlations.
# @param cex.exposures (when \code{type = "matrix"}; default \code{0.50}) Size of
# exposure' labels
# @param cex.family (when \code{type = "matrix"}; default \code{0.50}) Size of
# family's labels
# @param correlation.limits (when \code{type = "circos"}) List of parameters to
# control correation links. An example of each item in the list is:
# \code{list(d = '+', t = 0.5,  c = "#191970")}. Each item in the
# \code{correlation.limits} must be a list with e elements: direction (\code{d}),
# threshold \code{t} and color \code{c}. The direction must be \code{"+"} or
# \code{"-"} and sets the comparision (positive or negative). The threshold
# indicates the value of the comparison. The color indicates the color of the
# link that will be drawn. The example \code{list(d = '+', t = 0.5,  c = "#191970")}
# is understood as: The correlations over (\code{d="+"}) the threshold \code{0.5}
# will be coloured as \code{"#191970"}. The elements in the \code{correlation.limits}
# must be ordered and the comparisions are nested, wihout drawing the center interval.
# Default values for \code{correlation.limits} is:
#     \code{list(list(d = '+', t = 0.5,  c = "#191970"),
#        list(d = '+', t = 0.3,  c = "#4169E1"),
#        list(d = '-', t = -0.3, c = "#DC143C"),
#        list(d = '-', t = -0.5, c = "#8B0000"))}
# So the correlation over \code{0.5} will be drawn with dark-red. The correlations
# between \code{0.3} and 0.5 will be draws with bright-red. The correlations between
# -0.3 and 0.3 will not be drawn. The correlations between \code{-0.3} and -0.5
# will be draws with light-blue and the correlations under \code{-0.5} will
# be draws in dark-blue.
#' @return A \code{list} with different graphics parameters.
#' @examples
#' data("exposome")
#' expo.c <- correlation(expo)
#' plotCorrelation(expo.c, type="circos")
#' plotCorrelation(expo.c, type="matrix")
#' @export plotCorrelation
#' @seealso \link{correlation} as a constructor for \link{ExposomeCorr}
#' objects, \link{pca} to compute PCA on exposures
setGeneric("plotCorrelation", function(object, type = "circos", ...)
    standardGeneric("plotCorrelation")
)

# -----------------------------------------------------------------------------

#' Method to perform clustering on the samples of an ExposomeSet
#'
#' This method allows to create an \link{ExposomeClust} object from an
#' \link{ExposomeSet} object by clustering samples through the exposure
#' levels. The method is flexible to accept any clustering method
#' (\code{method}) that allows to obtain a classification (\code{cmethod})
#' of the samples. The function assigned to argument \code{method} must have
#' an argument called \code{data}, waiting for the matrix of exposures
#' (samples as rows, exposures as columns). If the result object of the
#' \code{method} has no accessor \code{$classification}, then a \code{cmethod}
#' is required and will be applied on the result of \code{method} to obtain
#' a labelled vector with the samples' classification.
#'
#' @name clustering
#' @rdname clustering-methods
#' @aliases clustering
#' @param object \code{ExposomeSet} containing the exposures used for the
#' clustering process
#' @param method Function applied to the exposures of \code{object}. This
#' function must has an argument named as \code{data} that will receive the
#' matrix of exposures.
#' @param cmethod (optional) Function to obtain the classification from the object
#' generated with \code{method}.
#' @param ... Passed to content of \code{method}.
#' @param warnings (default \code{TRUE}) If set to \code{FALSE} warnings will
#' not be displayed.
#' @return \code{ExposomeClust} with the original exposures and the
#' classification of each exposure.
#' @note The function assigned to \code{cmethod} will be directy applied to
#' the result of the \code{method} as: \code{cmethod(model)}; being
#' \code{model} the result of \code{method}.
#' @examples
#' data("exposome")
#'
#' # EXAMPLE 1: Clustering with mclust
#' library(mclust)
#' c <- clustering(expo[12:32, ], method = Mclust, G = 2)
#' table(classification(c))  # This works since the result of Mclust has an accessor
#'                    # $classification
#'
#' # EXAMPLE 2: Cluseting with flexmix
#' library(flexmix)
#' # First we carete a function to apply flexmix to the ExposomeSet
#' flexmix_clust <- function(data, ...) {
#'   data <- as.matrix(data)
#'   flexmix(formula = data~1, ...)
#' }
#'
#' # Then if we apply the method to the ExposomeSet it will crash:
#' # c <- clustering(expo[12:32, ], method = flexmix_clust, k = 2, model = FLXMCmvnorm())
#' # Because the method does not know how to obtain the classification for the result
#' # since flexmix has not an accessor called $classiciation
#'
#' # We create a function to get the classification
#' flexmix_clas <- function(model, ...) {
#'   return(clusters(model))
#' }
#'
#' # We put it to the ExposomeClust
#' c <- clustering(expo[12:32, ], method = flexmix_clust, cmethod = flexmix_clas,
#'     k = 2, model = FLXMCmvnorm())
#' classification(c) # This works because the ExposomeClust has a way to get
#'                   # the classification
#' @export clustering
#' @seealso \link{classification} to see how to obtain the classification of
#' the samples from an \link{ExposomeClust}, \link{plotClassification} to
#' plot the groups
setGeneric("clustering", function(object, method, cmethod, ..., warnings = TRUE)
    standardGeneric("clustering")
)

#' Method to get the classification of the samples from an ExposomeClust.
#'
#'
#' @name classification
#' @rdname classification-methods
#' @aliases classification
#' @param object An \link{ExposomeClust} to get the samples' classification.
#' @return A labelled vector with the classification of each exposure.
#' @examples
#' data("eclust")
#' tt <- classification(expo_c)
#' table(tt)
#' @export classification
#' @seealso \link{clustering} as a constructor for \link{ExposomeClust},
#' \link{plotClassification} to plot the groups
setGeneric("classification", function(object)
    standardGeneric("classification")
)

#' Draw the profile of the levels exposures after a classification with a
#' clustering method
#'
#' @name plotClassification
#' @rdname plotClassification-methods
#' @aliases plotClassification
#' @param object Object of class \code{Exposomeclust}
#' @param type Two types are available: \code{"heatmap"} or \code{"valuemap"}.
# @param scatter (if \code{type="valuemap"}; default \code{TRUE}) If set to
# \code{TRUE} shows the datapoints on the boxplot.
# @param cexRow (if \code{type="heatmap"}; default \code{0.5}) Size used
# on labelling rows
# @param cexCol (if \code{type="heatmap"}; default \code{1.1}) Size used
# on labelling columns
# @param adjCol (if \code{type="heatmap"}; default \code{c(0.5, 1)})
# Adjustments used for column label placment.
#' @param ... NOT USED
#' @return A \code{list} with different graphics parameters.
#' @examples
#' \dontrun{
#' data("eclust")
#' plotClassification(expo_c)
#' }
#' @export plotClassification
#' @seealso \link{clustering} as a constructor for \link{ExposomeClust},
#' \link{classification} to see how to obtain the classification of
#' the samples from an \link{ExposomeClust}
setGeneric("plotClassification", function(object, type = "heatmap", ...)
    standardGeneric("plotClassification")
)

# -----------------------------------------------------------------------------
#' Testing the association between an exposure and a phenotype of an
#' ExposomeSet using a multivariate aproach.
#'
#' The \code{mexwas} method performs an "Multi Exposome-Wide
#' Association Study" (m-ExWAS) using the exposures in \link{ExposomeSet}
#' and one of its phenotype. It uses the packages \code{glmnet} and
#' \code{partDSA}.
#'
#' @name mexwas
#' @rdname mexwas-methods
#' @aliases mexwas
#' @param object \code{ExposomeSet} that will be used for the ExWAS.
#' @param phenotype Target phenotype used for the study. If missing all the
#' phenotypes in the \link{ExposomeSet} will be used.
#' @param family It must decrive the nature of the outcome. Can take values
#' \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"multinomial"},
#' \code{"cox"} or \code{"mgaussian"}.
#' @param warnings (default \code{TRUE}) If set to \code{FALSE} warnings will
#' not be displayed.
#' @return Returns an object of class \link{mExWAS}
#' @examples
#' data("exposome")
#' wt <- mexwas(expo[3:7, 1:100], phenotype = "asthma", family = "binomial")
#' @export mexwas
#' @seealso \link{extract} to obtain a table with the result of the ExWAS,
#' \link{plotExwas} to plot the results of the ExWAS
setGeneric("mexwas", function(object, phenotype, family, warnings = TRUE)
    standardGeneric("mexwas")
)


# -----------------------------------------------------------------------------

#' Testing the association between an exposure and a phenotype of an
#' ExposomeSet
#'
#' The \code{exwas} method performs an "Exposome-Wide Association Study" (ExWAS)
#' using the exposures in \link{ExposomeSet} and one of its phenotype.
#'
#' @name exwas
#' @rdname exwas-methods
#' @aliases exwas
#' @param object \code{ExposomeSet} that will be used for the ExWAS.
#' @param formula \code{formula} indicating the test to be done. If any
#' exposure is included it will be used as covariate. \code{exwas} metho will
#' perform the test for each exposure.
#' @param filter \code{expression} to be used to filter the individuals
#' included into the test.
#' @param family Family of the distribution followed by the health outcome to
#' be tested (gaussian, bionomal, ... check \link{glm}).
#' @param tef (default \code{TRUE}) If \code{TRUE} it computed the
#' effective number of tests and the threhold for the effective
#' number of tests. Usually it needs imputed data.
#' @param ... NOT USED
#' @param verbose (default \code{FALSE}) If set o true messages along the
#' tests are shown.
#' @param warnings (default \code{TRUE}) If set to \code{FALSE} warnings will
#' not be displayed.
#' @return An code{ExWAS} object with the result of the association study
#' @references An Environment-Wide Association Study (ExWAS) on Type 2 Diabetes
#' Mellitus. Chirag J. Patel, Jayanta Bhattacharya, Atul J. Butte. May 20, 2010
#' Plos One
#' @references Evaluating the effective numbers of independent tests and
#' significant p-value thresholds in commercial genotyping arrays and public
#' imputation reference datasets. Miao-Xin Li, Juilian M. Y. Yeung,
#' Stacey S. Cherny and Pak C. Sham. May 2012 Hum Genet.
#' @examples
#' data(exposome)
#' w1 <- exwas(expo[1:5, ], asthma~1, family = "binomial")
#' w2 <- exwas(expo[1:5, ], asthma~sex+age, family = "binomial")
#' w3 <- exwas(expo[1:5, ], asthma~age, family = "binomial", filter = sex=="boy")
#' @export exwas
#' @seealso \link{extract} to obtain a table with the result of the ExWAS,
#' \link{plotExwas} to plot the results of the ExWAS
setGeneric("exwas", function(object, formula, filter, family, ..., tef = TRUE, verbose = FALSE, warnings = TRUE)
    standardGeneric("exwas")
)

#' Function to draw a plot of the pvalues stored in an \code{ExWAS} object
#'
#' This function draws a sort of manhattan plots using the p-value of the
#' association of the exposures with phenotypes of an \code{ExWAS} object.
#'
#' @name plotExwas
#' @rdname plotExwas-methods
#' @aliases plotExwas
#' @param object An \code{ExWAS} object which p-values will be plotted.
#' @param ... Other objects of class \code{ExWAS}.
#' @param subtitles (optional) Characters used as "substitle" when more than one
#' \code{ExWAS} is given.
#' @param color (optional) Character vector of HTML colors, labeled with
#' family's names. Used to colore the exposures.
#' @param exp.order (optional) Character vector of exposures used to order
#' and subset the plot.
#' @param show.effective (default \code{TRUE}) If set to \code{FALSE},
#' line showing effective test threshold is not shown.
#' @return An object of class \code{ggplot}.
#' @examples
#' data(exposome)
#' w1 <- exwas(expo[1:5, ], asthma~1, family = "binomial")
#' plotExwas(w1)
#' @export plotExwas
#' @seealso \link{exwas} as a constructor for \link{ExWAS} objects,
#' \link{extract} to obtain a table with the result of the ExWAS
setGeneric("plotExwas", function(object, ..., subtitles, color, exp.order, show.effective = TRUE)
    standardGeneric("plotExwas")
)

#' Function to draw a plot of the pvalues stored in an \code{ExWAS} object
#'
#' This function draws a sort of manhattan plots using the p-value of the
#' association of the exposures with phenotypes of an \code{ExWAS} object.
#'
#' @name plotEffect
#' @rdname plotEffect-methods
#' @aliases plotEffect
#' @param x An \code{ExWAS} object which effect will be ploted.
#' @param y (optional) Another \code{ExWAS} object. If provded its
#' effects will be ploted in Y-axis.
#' @param select (optional) Character with exposures to be shown.
#' @param xlab (optional) Label for X-axis.
#' @param ylab (optional) Label for Y-axis.
#' @return An object of class \code{ggplot}.
#' @examples
#' data(exposome)
#' w1 <- exwas(expo[1:5, ], asthma~1, family = "binomial")
#' w2 <- exwas(expo[1:5, ], asthma~sex+age, family = "binomial")
#' plotEffect(w1, w2)
#' @export plotEffect
#' @seealso \link{exwas} as a constructor for \link{ExWAS} objects,
#' \link{extract} to obtain a table with the result of the ExWAS
setGeneric("plotEffect", function(x, y, select, xlab, ylab)
    standardGeneric("plotEffect")
)

#' Function to draw a plot of the pvalues stored in an \code{ExWAS} object
#'
#' This function draws a sort of manhattan plots using the p-value of the
#' association of the exposures with phenotypes of an \code{ExWAS} object.
#'
#' @name plotVolcano
#' @rdname plotVolcano-methods
#' @aliases plotVolcano
#' @param x An \code{ExWAS} object which effect will be ploted.
#' @param p.value (default \code{"-log10(0.001)"}) Threshold for P-Value.
#' @param show.effect (default \code{FALSE}) Applyes an exponential
#' transformation on the effects of the exposures.
#' @return An object of class \code{ggplot}.
#' @export plotVolcano
#' @seealso \code{\link{exwas}} as a constructor for \code{\link{ExWAS}}
#' objects, \code{\link{extract}} to obtain a table with the result of
#' the ExWAS, \code{\link{plotEffect}} to see or compare effects of
#' one or two models.
setGeneric("plotVolcano", function(x, p.value = -log10(0.001), show.effect = FALSE)
    standardGeneric("plotVolcano")
)

#' Function to get the Threshold for effective tests (TEF)
#'
#' @name tef
#' @rdname tef-methods
#' @aliases tef
#' @param object An \code{ExWAS} object
#' @examples
#' data(exposome)
#' w1 <- exwas(expo[1:5, ], asthma~1, family = "binomial")
#' w2 <- exwas(expo[1:5, ], asthma~sex+age, family = "binomial")
#' tef(w1)
#' tef(w2)
#' @return A number indicating the efective threshold.
#' @export tef
#' @seealso \link{exwas} as a constructor for \link{ExWAS} objects
#' @references Evaluating the effective numbers of independent tests and
#' significant p-value thresholds in commercial genotyping arrays and public
#' imputation reference datasets. Miao-Xin Li, Juilian M. Y. Yeung,
#' Stacey S. Cherny and Pak C. Sham. May 2012 Hum Genet.
setGeneric("tef", function(object)
    standardGeneric("tef")
)


#' Method to convert an imExposomeSet to an ExposomeSet
#'
#' This methods allows to select an imputed-set and use it to create an
#' \code{\link{ExposomeSet}} from an \code{\link{imExposomeSet}}.
#'
#' @name toES
#' @rdname toES
#' @aliases toES
#' @param object An object of class \code{imExposomeSet}
#' @param rid (default \code{1}) Number of the imputation to be extracted
#' @return An object of class \code{\link{ExposomeSet}}-
#' @export toES
#' @examples
#' data("ex_imp")
#' toES(ex_imp, rid = 1)
setGeneric("toES", function(object, rid = 1)
    standardGeneric("toES")
)
