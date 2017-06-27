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
#' @param exposures.samCol (default \code{1}) Index where the samples' name
#' are found in file "exposures".
#' @param description.expCol (default \code{1}) Index where the exposures'
#' name are found in file "description".
#' @param description.famCol (default \code{2}) Index where the family's
#' name (per exposures) if found in file "description".
#' @param phenotype.samCol (default \code{1}) Index where the sample's name
#' are found in file "phenotype".
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
#' path <- paste0(path.package("rexposome"), .Platform$file.sep, "extdata")
#' description <- paste0(path, .Platform$file.sep, "description.csv")
#' phenotype <- paste0(path, .Platform$file.sep, "phenotypes.csv")
#' exposures <- paste0(path, .Platform$file.sep, "exposures.csv")
#'
#' ## Create ExposomeSet from files
#' exp <- read_exposome(
#'   exposures = exposures,
#'   description = description,
#'   phenotype = phenotype,
#'   exposures.samCol = 1,
#'   description.expCol = 2,
#'   description.famCol = 1,
#'   phenotype.samCol = 1
#' )
#' @export read_exposome
#' @seealso \link{ExposomeSet} for class description,
#' \link{load_exposome} for constructor from loaded
#' \code{data.frame}s
read_exposome <- function(exposures, description, phenotype,
    sep = ",", na.strings = c("NA", "-", "?", " ", ""),
    exposures.samCol = 1, description.expCol = 1, description.famCol = 2,
    phenotype.samCol = 1, exposures.asFactor = 5,
    warnings = TRUE, ...) {

    ## Load the three dataframes
    exp <- utils::read.table(exposures, header = TRUE,
        row.names = exposures.samCol, sep = sep, na.strings = na.strings)
    phe <- utils::read.table(phenotype, header = TRUE,
        row.names = phenotype.samCol, sep = sep, na.strings = na.strings,
        stringsAsFactors = TRUE)
    desc <- utils::read.table(description, header = TRUE,
        row.names = description.expCol, sep = sep, na.strings = na.strings)
    if(description.famCol > description.expCol) {
        description.famCol <- description.famCol - 1
    }
    ## ------------------------------------------------------------------------

    exposome <- load_exposome(exp, desc, phe, description.famCol, exposures.asFactor,
                  warnings)

    return(exposome)
}
