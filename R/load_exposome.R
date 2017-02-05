#' Creation of an ExposomeSet from \code{data.frames}
#'
#' Given three \code{data.frames} that defines the exposome (measures
#' of exposome, exposome description and individuals phentype) it loads
#' them and creates an object of type \link{ExposomeSet}.
#'
#' The rows of the exposure's \code{data.frames}, that corresponds to samples'
#' names, must be the same than the phenotype's \code{data.frames}. In the
#' same way, the columns in exposure's \code{data.frames} must be the same
#' in description \code{data.frame}.
#'
#' @param exposures \code{data.frame} of exposures.
#' @param description \code{data.frame} with the description of the exposures
#' (relation between exposures and exposure-family).
#' @param phenotype \code{data.frame} with the phenotypes of interest.
#' @param description.famCol (default \code{1}) Index where the family's
#' name (per exposures) if found in file "description".
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
#' @note \link{ExposmeSet}'s \code{fData} will contain two inner columns called
#' \code{_std}, \code{_trn}, \code{_fct} and \code{_type} in order to trace the
#' transformations an exposure suffers and to know, at eny moment, if an
#' exposure is categorical or continuous. The "description" file can contains a
#' column called \code{type} with values \code{"factor"} and \code{"numeric"}
#' to speficy how an exposure needs to be understood. If given, this column
#' will be renamed to \code{_type}. If not given, it will be created using
#' \code{exposures.asFactor} value.
#' @export load_exposome
#' @seealso \link{ExposomeSet} for class description,
#' \link{read_exposome} for constructor from txt/csv
#' files.
load_exposome <- function(exposures, description, phenotype,
                          description.famCol = 1, exposures.asFactor = 5,
                          std.e = c("none", "normal", "robust"), trn.e = "none",
                          warnings = TRUE, ...) {

    ## Order the colmuns on description
    ##   rownames <- exposures
    ##   column  1 must be the family
    description.famCol <- colnames(description)[description.famCol]
    description <- description[ , c(description.famCol,
                      colnames(description)[colnames(description) != description.famCol]), drop=FALSE]
    colnames(description)[1] <- "Family"
    ## ------------------------------------------------------------------------

    ## Check for inner names on description
    ##   description cannot contain _status nor _type
    if("_std" %in% colnames(description)) {
        stop("Given descriptiion dat contains '_std' as name of a column. ",
             "Name '_std' cannot be used in 'ExposmeSet'.")
    }

    if("_trn" %in% colnames(description)) {
        stop("Given descriptiion dat contains '_trn' as name of a column. ",
             "Name '_trn' cannot be used in 'ExposmeSet'.")
    }

    if("_fct" %in% colnames(description)) {
        stop("Given descriptiion dat contains '_fct' as name of a column. ",
             "Name '_fct' cannot be used in 'ExposmeSet'.")
    }

    if("_imp" %in% colnames(description)) {
        stop("Given descriptiion dat contains '_imp' as name of a column. ",
             "Name '_imp' cannot be used in 'ExposmeSet'.")
    }

    if("_type" %in% colnames(description)) {
        stop("Given descriptiion dat contains '_type' as name of a column. ",
             "Name '_type' cannot be used in 'ExposmeSet'.")
    }
    ## ------------------------------------------------------------------------

    ## Check that the exposures and the samples from the three dataframes
    ## are the same
    exp.col <- colnames(exposures)
    exp.col <- exp.col[order(exp.col)]
    exp.row <- rownames(exposures)
    exp.row <- exp.row[order(exp.row), drop=FALSE]
    exposures <- exposures[exp.row, exp.col]

    des.row <- rownames(description)
    des.row <- des.row[order(des.row)]
    description <- description[des.row, , drop=FALSE]

    phe.row <- rownames(phenotype)
    phe.row <- phe.row[order(phe.row)]
    phenotype <- phenotype[phe.row, , drop=FALSE]

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
    description$`_fct` <- ""
    description$`_trn` <- ""
    description$`_std` <- ""
    description$`_imp` <- ""
    ## ------------------------------------------------------------------------

    ## Need to create and fill _type of description
    if("type" %in% colnames(description)) {
        if(warnings) {
            warning("Fund colnames 'type' in description file. It will be ",
                    "used to check for exposures' type. Then 'type' column ",
                    "will be droped.")
        }
        description$type <- as.character(description$type)
        if(sum(unique(description$type) %in% c("numeric", "factor")) != 2) {
            stop("In 'type' column of description file only 'factor' or ",
                 "'numeric' calues can be used.")
        }
        description$`_type` <- description$type
        description <- description[ , -which(colnames(description) == "type")]
    } else {
        description$`_type` <- sapply(rownames(description), function(ex) {
            ifelse(length(unique(exposures[ , ex])) > exposures.asFactor, "numeric", "factor")
        })
    }
    ## ------------------------------------------------------------------------

    ## Exposures must be saved as numeric-matrix
    exposures <- (as.matrix(exposures))
    ## ------------------------------------------------------------------------

    ## Create and validate ExposomeSet
    exposome <- new("ExposomeSet",
                    assayData = assayDataNew("environment", raw = t(exposures), exp = t(exposures)),
                    phenoData = AnnotatedDataFrame(phenotype),
                    featureData = AnnotatedDataFrame(description)
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
