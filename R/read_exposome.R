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
#' @param std.e If specified, it will be the method used to standardize
#' the exposures. It can be set to \code{"normal"}, where mean/sd will be used,
#' or to \code{"robust"}, where median/mad will be used (see
#' \link{standardize}).
#' @param trn.e If specified it can contain a names vector of functions to be
#' applied to the exposures's \code{data.frame} (see \link{transform}).
#' @param warnings (default \code{TRUE}) If \code{TRUE} shows useful
#' information/warnings from the process of loading the exposome.
#' @param ... Other arguments will be given to \link{transform} if \code{trn.e}
#' is specified.
#' @return An object of class \link{ExposomeSet}.
#' @note \link{ExposomeSet}'s \code{fData} will contain two inner columns called
#' \code{.std}, \code{.trn}, \code{.fct} and \code{.type} in order to trace the
#' transformations an exposure suffers and to know, at eny moment, if an
#' exposure is categorical or continuous. The "description" file can contains a
#' column called \code{type} with values \code{"factor"} and \code{"numeric"}
#' to speficy how an exposure needs to be understood. If given, this column
#' will be renamed to \code{.type}. If not given, it will be created using
#' \code{exposures.asFactor} value.
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
    std.e = c("none", "normal", "robust"), trn.e = "none",
    warnings = TRUE, ...) {

    ## Load the three dataframes
    exp <- utils::read.table(exposures, header = TRUE, row.names = exposures.samCol,
                    sep = sep, na.strings = na.strings)
    phe <- utils::read.table(phenotype, header = TRUE, row.names = phenotype.samCol,
                    sep = sep, na.strings = na.strings, stringsAsFactors = TRUE)
    desc <- utils::read.table(description, header = TRUE, row.names = description.expCol,
                     sep = sep, na.strings = na.strings)
    if(description.famCol > description.expCol) {
        description.famCol <- description.famCol - 1
    }
    ## ------------------------------------------------------------------------

    ## Order the colmuns on description
    ##   rownames <- exposures
    ##   column  1 must be the family
    description.famCol <- colnames(desc)[description.famCol]
    desc <- desc[ , c(description.famCol,
                      colnames(desc)[colnames(desc) != description.famCol]), drop=FALSE]
    colnames(desc)[1] <- "Family"
    ## ------------------------------------------------------------------------

    ## Check for inner names on description
    ##   description cannot contain _status nor _type
    if(".std" %in% colnames(desc)) {
        stop("Given descriptiion dat contains '.std' as name of a column. ",
             "Name '.std' cannot be used in 'ExposmeSet'.")
    }

    if(".trn" %in% colnames(desc)) {
        stop("Given descriptiion dat contains '.trn' as name of a column. ",
             "Name '.trn' cannot be used in 'ExposmeSet'.")
    }

    if(".fct" %in% colnames(desc)) {
        stop("Given descriptiion dat contains '.fct' as name of a column. ",
             "Name '.fct' cannot be used in 'ExposmeSet'.")
    }

    if(".imp" %in% colnames(desc)) {
        stop("Given descriptiion dat contains '.imp' as name of a column. ",
             "Name '.imp' cannot be used in 'ExposmeSet'.")
    }

    if(".type" %in% colnames(desc)) {
        stop("Given descriptiion dat contains '.type' as name of a column. ",
             "Name '.type' cannot be used in 'ExposmeSet'.")
    }
    ## ------------------------------------------------------------------------

    ## Check that the exposures and the samples from the three dataframes
    ## are the same
    exp.col <- colnames(exp)
    exp.col <- exp.col[order(exp.col)]
    exp.row <- rownames(exp)
    exp.row <- exp.row[order(exp.row), drop=FALSE]
    exp <- exp[exp.row, exp.col]

    des.row <- rownames(desc)
    des.row <- des.row[order(des.row)]
    desc <- desc[des.row, , drop=FALSE]

    phe.row <- rownames(phe)
    phe.row <- phe.row[order(phe.row)]
    phe <- phe[phe.row, , drop=FALSE]

    if(!identical(exp.col, des.row)) {
        stop("Exposures's names in exposures and in description files ",
            "don't match.")
    }
    if(!identical(exp.row, phe.row)) {
        stop("Samples's names in exposures and in phenotype files don't match.")
    }
    rm(exp.col, exp.row, des.row, phe.row)
    ## ------------------------------------------------------------------------

    ## Need to create _status
    desc$`.fct` <- ""
    desc$`.trn` <- ""
    desc$`.std` <- ""
    desc$`.imp` <- ""
    ## ------------------------------------------------------------------------

    ## Need to create and fill _type of description
    if("type" %in% colnames(desc)) {
        if(warnings) {
            warning("Fund colnames 'type' in description file. It will be ",
                    "used to check for exposures' type. Then 'type' column ",
                    "will be droped.")
        }
        desc$type <- as.character(desc$type)
        if(sum(unique(desc$type) %in% c("numeric", "factor")) != 2) {
            stop("In 'type' column of description file only 'factor' or ",
                 "'numeric' calues can be used.")
        }
        desc$`.type` <- desc$type
        desc <- desc[ , -which(colnames(desc) == "type"), drop=FALSE]
    } else {
        desc$`.type` <- sapply(rownames(desc), function(ex) {
            ifelse(length(unique(exp[ , ex])) > exposures.asFactor, "numeric", "factor")
        })
    }
    ## ------------------------------------------------------------------------

    ## Exposures must be saved as numeric-matrix
    exp <- (as.matrix(exp))
    ## ------------------------------------------------------------------------

    ## Create and validate ExposomeSet
    exposome <- new("ExposomeSet",
        assayData = assayDataNew("environment", raw = t(exp), exp = t(exp)),
        phenoData = AnnotatedDataFrame(phe),
        featureData = AnnotatedDataFrame(desc)
    )

    validObject(exposome)
    ## ------------------------------------------------------------------------

    ## Standardization and transformation
    std.e <- match.arg(std.e)
    if(std.e != "none") {
        exposome <- standardize(exposome, std.e)
    }

    if(!missing(trn.e)) {
        exposome <- transform(exposome, fun = trn.e, ...)
    }
    ## ------------------------------------------------------------------------

  return(exposome)
}
