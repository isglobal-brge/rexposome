#' Creation of an imExposomeSet from \code{data.frames}
#'
#' Given a \code{data.frame} from \code{code} with the multiple
#' imputations of both exposures and phenotypes, join with a \code{data.frame}
#' with exposures' description, and object of class \code{\link{imExposomeSet}}
#' is created.
#'
#' The coordination of the information is based in the columns \code{.imp} and
#' \code{.id} from the \code{data.frame} obtained from \code{mice}. The
#' division of exposures and phenotypes is based in description
#' \code{data.frame}, that are the exposures. Hence, the elements in the
#' main \code{data.frame} that are not in the description, are the
#' phentoypes.
#'
#' @param data The \code{data.frame} of both exposures and phentoypes obtained
#' from \code{mice}.
#' @param description \code{data.frame} with the description of the exposures
#' (relation between exposures and exposure-family).
#' @param description.famCol (default \code{1}) Index where the family's
#' name (per exposures) if found in file "description".
#' @param description.expCol (default \code{2}) Index where the exposure's
#' name if found in file "description".
#' @param exposures.asFactor (default \code{5}) The exposures with more
#' than this number of unique items will be considered as "continuous" while
#' the exposures with less or equal number of items will be considered as
#' "factor".
#' @return An object of class \link{imExposomeSet}.
#' @export load_imputed
#' @seealso \link{imExposomeSet} for class description
#' @examples
#' data("me") # me is an imputed matrix of exposure and phenotyes
#' path <- paste0(path.package("rexposome"), .Platform$file.sep, "extdata")
#' description <- paste0(path, .Platform$file.sep, "description.csv")
#' dd <- read.csv(description, header=TRUE, stringsAsFactors=FALSE)
#' dd <- dd[dd$Exposure %in% colnames(me), ]
#' ex_imp <- load_imputed(data = me, description = dd,
#' description.famCol = 1,
#' description.expCol = 2)
load_imputed <- function(data, description, description.famCol = 1,
                         description.expCol = 2, exposures.asFactor = 5) {

    if(!".imp" %in% colnames(data)) {
        stop("Given imputed exposures matrix has no column '.imp'.")
    }
    if(!".id" %in% colnames(data)) {
        stop("Given imputed exposures matrix has no column '.id'.")
    }

    # Order the colmuns on description
    ##   column  1 must be the family
    ##   column  2 must be the exposures
    description.famCol <- colnames(description)[description.famCol]
    description.expCol <- colnames(description)[description.expCol]
    description <- description[ , c(description.famCol, description.expCol,
                                    colnames(description)[!colnames(description) %in% c(description.famCol, description.expCol)]), drop=FALSE]
    colnames(description)[1] <- "Family"
    colnames(description)[2] <- "Exposure"
    rownames(description) <- description$Exposure
    ## ------------------------------------------------------------------------

    ## Split the exposures from the phenotypes ar data data.frame
    exp.names <- c(".imp", ".id", rownames(description))
    phe.names <- colnames(data)[!colnames(data) %in% rownames(description)]

    exposures <- data[ , exp.names, drop=FALSE]
    phenotypes <- data[ , phe.names, drop=FALSE]

    #exposures[ , 1] <- as.numeric(exposures[ , 1])
    exposures[ , 2] <- as.character(exposures[ , 2])

    #phenotypes[ , 1] <- as.numeric(phenotypes[ , 1])
    phenotypes[ , 2] <- as.character(phenotypes[ , 2])

    exposures <- exposures[order(exposures[ , 1], exposures[, 2]), ]
    phenotypes <- phenotypes[order(phenotypes[ , 1], phenotypes[, 2]), ]

    rownames(exposures) <- 1:nrow(exposures)
    rownames(phenotypes) <- 1:nrow(phenotypes)

    rm(exp.names, phe.names)
    ## ------------------------------------------------------------------------


    ## Need to create and fill .type of description
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
        description$`.type` <- description$type
        description <- description[ , -which(colnames(description) == "type"), drop=FALSE]
    } else {
        description$`.type` <- sapply(rownames(description), function(ex) {
            ifelse(length(unique(exposures[ , ex])) > exposures.asFactor, "numeric", "factor")
        })
    }
    ## ------------------------------------------------------------------------

    ## ------------------------------------------------------------------------

    ## Create and validate ExposomeSet
    exposome <- new("imExposomeSet",
                    nimputation = length(unique(exposures[, 1])) - 1,
                    assayData = S4Vectors::DataFrame(exposures),
                    phenoData = S4Vectors::DataFrame(phenotypes),
                    featureData = S4Vectors::DataFrame(description)
    )

    return(exposome)
}
