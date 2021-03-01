#' Creation of an ExposomeSet from single \code{data.frame}
#'
#' @param data \code{data.frame} With the exposures and phenotypes (in no particular order!) or 
#' \code{string} with the path to a file (.csv, .tsv, .txt) with the table of exposures and phenotypes
#' @param data_id \code{character} Name of the column on the selected table that contains the ID
#' @param sep \code{character} (default \code{","}) Separator used by \code{\link{read.table}} to
#' load the files "exposures", "description" and "phenotype". Only applies when providing a path on the 
#' data argument
#' @param pheno_cols \code{character} Character vector of the phenotype columns (all the other columns 
#' are considered exposures)
#' @param na.strings \code{character} (default \code{c("NA", "-", "?", " ", "")}) Character
#' defining the \code{NA} values in expsome's files.
#' @param families \code{list} (default \code{NULL}) List to specify the families of the exposures,
#' construct it as: \code{list(Family1 = c("exposure_1_1", "exposure_1_2", "exposure_1_n"), Family2 = 
#' c("exposure_2_1", "exposure_2_2", "exposure_2_n"), FamilyM = c("exposure_M_1", "exposure_M_2", "exposure_M_n"))}. 
#' All the exposures on the data table have to be on this provided list with their respective families. 
#' The family classification is optional, input \code{NULL} to bypass the family classifier
#' @param exposures.asFactor \code{numeric} (default \code{5}) The exposures with more
#' than this number of unique items will be considered as "continuous" while
#' the exposures with less or equal number of items will be considered as
#' "factor".
#' @param warnings (default \code{TRUE}) If \code{TRUE} shows useful
#' information/warnings from the process of loading the exposome.
#'
#' @return An object of class \link{ExposomeSet}.
#' @export
#'
#' @examples
#' path <- file.path(path.package("rexposome"), "extdata")
#' phenotype <- file.path(path, "phenotypes.csv")
#' exposures <- file.path(path, "exposures.csv")
#' ee <- read.csv(exposures, header=TRUE)
#' pp <- read.csv(phenotype, header=TRUE)
#' # Create fake dataset with exposures and phenotypes combined
#' data <- cbind(ee, pp)
#' 
#' loadExposome_plain <- function(data, data_id = "idnum",
#' pheno_cols = c("rhinitis", "wheezing", "sex", "age", "cbmi", "blood_pre", "whistling_chest","flu"))
#' 

loadExposome_plain <- function(data, data_id, sep = ",", pheno_cols, 
                               na.strings = c("NA", "-", "?", " ", ""),
                               families = NULL, exposures.asFactor = 5, warnings = TRUE){
  
  if(class(data) == "data.frame"){is_path <- FALSE}
  else if(class(data) == "character"){is_path <- TRUE}
  
  if(is_path){
    data <- utils::read.table(data, header = TRUE,
                             row.names = data_id, sep = sep, na.strings = na.strings)
  }
  
  exposures <- data[, !(colnames(data) %in% pheno_cols)]
  phenotype <- data[, pheno_cols]
  
  if(is.null(families)){
    description <- data.frame(Family = colnames(exposures), Exposure = colnames(exposures), Name = NA)
  }
  else{
    # Check that all exposures are present on the supplied families list,
    # if they are not, the exposomeSet creation will fail
    fams <- unlist(families)
    if(!all(fams %in% colnames(exposures))){
      stop("Missing exposures on the families list")
    }
    
    description <- reshape2::melt(do.call(cbind, families))[,2:3]
    colnames(description) <- c("Family", "Exposure")
    description <- cbind(description, Name = NA)
  }
  rownames(description) <- description$Exposure
  
  exposome <- loadExposome( exposures = exposures, description = description,
                            phenotype = phenotype, description.famCol = "Family", exposures.asFactor,
                            warnings )
  return(exposome)
}
