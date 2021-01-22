#' Creation of an ExposomeSet from files
#'
#' Given the files that defines the exposome (measures of exposome, exposome
#' description and individuals phentype) it loads them and creates an
#' object of type \link{ExposomeSet}.
#'
#' The rows of the exposure's file, that corresponds to samples' names, must
#' be the same than the phenotype's file. In the same way, the columns in
#' exposure's file must be the same found as rows in description file.
#'
#' @param exposures String with the path to the file with the matrix of
#' exposures.
#' @param description String with the path to the file with the description of
#' the exposures (relation between exposures and exposure-family).
#' @param phenotype String with the path to the file with the phenotypes of
#' interest.
#' @param sep (default \code{","}) Separator used by \code{\link{read.table}} to
#' load the files "exposures", "description" and "phenotype".
#' @param na.strings (default \code{c("NA", "-", "?", " ", "")}) Character
#' defining the \code{NA} values in expsome's files.
#' @param exposures.samCol (default \code{"sample"}) Index where the samples'
#' name are found in file "exposures". It can be both charatcer or numeric.
#' @param description.expCol (default \code{"exposure"}) Index where the
#' exposures' name are found in file "description". It can be both numeric
#' or character.
#' @param description.famCol (default \code{"family"}) Index where the family's
#' name (per exposures) if found in file "description". It can be both numeric
#' or character.
#' @param phenotype.samCol (default \code{"sample"}) Index where the sample's
#' name are found in file "phenotype". It can be both numeric or character.
#' @param exposures.asFactor (default \code{5}) The exposures with more
#' than this number of unique items will be considered as "continuous" while
#' the exposures with less or equal number of items will be considered as
#' "factor".
#' @param warnings (default \code{TRUE}) If \code{TRUE} shows useful
#' information/warnings from the process of loading the exposome.
#' @return An object of class \link{ExposomeSet}.
#' @note \link{ExposomeSet}'s \code{fData} will contain some inner columns
#' called \code{.std}, \code{.trn}, \code{.fct} and \code{.type} in order to
#' trace the transformations an exposure suffers and to know, at eny moment, if
#' an exposure is categorical or continuous. The "description" file can
#' contains a column called \code{type} with values \code{"factor"} and
#' \code{"numeric"} to speficy how an exposure needs to be understood. If
#' given, this column will be renamed to \code{.type}. If not given, it will
#' be created using \code{exposures.asFactor} value.
#' @examples
#' ## Locate the data-files
#' path <- file.path(path.package("rexposome"), "extdata")
#' description <- file.path(path, "description.csv")
#' phenotype <- file.path(path, "phenotypes.csv")
#' exposures <- file.path(path, "exposures.csv")
#'
#' ## Create ExposomeSet from files
#' exp <- readExposome(
#'   exposures = exposures,
#'   description = description,
#'   phenotype = phenotype,
#'   exposures.samCol = 1,
#'   description.expCol = 2,
#'   description.famCol = 1,
#'   phenotype.samCol = 1
#' )
#' @export readExposome
#' @seealso \link{ExposomeSet} for class description,
#' \link{loadExposome} for constructor from loaded
#' \code{data.frame}s
readExposome <- function(exposures, description, phenotype,
    sep = ",", na.strings = c("NA", "-", "?", " ", ""),
    exposures.samCol = "sample", description.expCol = "exposure",
    description.famCol = "family", phenotype.samCol = "sample",
    exposures.asFactor = 5, warnings = TRUE) {

    ## Load the three dataframes
    exp <- utils::read.table(exposures, header = TRUE,
        row.names = exposures.samCol, sep = sep, na.strings = na.strings)
    phe <- utils::read.table(phenotype, header = TRUE,
        row.names = phenotype.samCol, sep = sep, na.strings = na.strings,
        stringsAsFactors = TRUE)
    desc <- utils::read.table(description, header = TRUE,
        row.names = description.expCol, sep = sep, na.strings = na.strings)
    ## ------------------------------------------------------------------------

    exposome <- loadExposome( exposures = exp, description = desc,
        phenotype = phe, description.famCol, exposures.asFactor,
                  warnings )

    return(exposome)
}
