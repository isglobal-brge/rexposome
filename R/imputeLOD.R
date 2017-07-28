#' Function to impute values under limit of detection
#'
#' Homologous function to the \link{ilod} method of \link{ExposomeSet} for a
#' given \code{data.frame} of exposures and a \code{vector} of threshold.
#' The use of this function is related to HELIX Project.
#'
#' @param x \code{data.frame} containing the exposures as columns and the
#' samples as rows.
#' @param lod \code{vectro} containing the
#' @param description \code{1} means values under LOD while \code{2} means
#' quantifiable value, \code{3}
#' @param pNA (default: \code{0.2}) maximum percentage of allowed missing data
#' @param pLOD (default: \code{0}) minimum percentave of values under LOD
#' @param log (default: \code{NA}) log transformation to normalize data
#' @param seed (default: \code{NULL})
#' @return A new \code{data.frame} with the imputed exposures.
#' @export imputeLOD
#' @examples
#' \dontrun{
#' inma.imp <- imputeLOD(x = raw, lod = lod, description = desc,
#'      pNA = pNA, pLOD = pLOD, log = log, seed = seed)
#' }

imputeLOD <- function(x, lod, description = NULL, pNA = 0.2, pLOD = 0,
    log = TRUE, seed = NULL) {
  #
  # Checkings:
  #

  samples_id <- colnames(x)

  rnames <- rownames(x)
  nx <- nrow(x)
  if(nx == 0) stop("There aren't variables to impute!")

  nlod <- length(lod)
  if(nlod == 0) warning("\n There is no information about LOD values!\n
  Imputations will be applied considering NA values as left-censored missings.")

  # Required R package:
  ## require(imputeLCMD)

  if(sum(rnames %in% names(lod)) < nx) {
      warning("\n There are some variables without LOD information \n. These variables are: ",
              rnames[!rnames %in% names(lod)])
    }

  if(sum(rnames %in% names(lod)) < nlod){
    warning("\n There are LOD values for missing variables. \n Imputations will not be done for these variables.\n The variables are: ",
            names(lod)[!names(lod) %in% rnames])
  }


  # Select variables (proteins) to impute:
  proteins <- rnames[order(rnames)]

  # Proteins with LOD value:
  proteins_wlod <- intersect(proteins, names(lod))

  if(log)
  {
    # Filter variables whose log tranformation can't be calculated:
    log_exclude <- unlist(apply(x[proteins,], 1,
                          function(x) class(try(log(x), TRUE)) == "try-error"))
    proteins_to_exclude <- proteins[log_exclude ]
  }


  # Proportion of values below LOD:

  if(nlod > 0){
     faux <- function(x, lod){ as.double(mean(x < lod, na.rm = TRUE)) }
     proteins_pLOD <- base::mapply(faux,
                                x = as.list(as.data.frame(t(x[proteins_wlod,]))),
                                lod = as.list(lod[proteins_wlod]))
       proteins_pLOD <- data.frame(protein = as.character(proteins_wlod), pLOD = proteins_pLOD)
  } else{
    proteins_pLOD <- NULL
  }


  # Missing indicator (matrix) not due to LOD to (after) assign NA values at imputed database:

  if(!is.null(description))  {
    faux <- function(x) !(x == "Values below <LOD" | x == 2 | x == "Quantifiable value" | x == 1)
    no_LOD_NA_indicator <- do.call(rbind,
      base::lapply(as.list(as.data.frame(t(description[proteins_wlod,]))), faux))
    colnames(no_LOD_NA_indicator) <- samples_id
  }
  else {
   xaux <- x[proteins_wlod,]
   no_LOD_NA_indicator <- matrix(FALSE, nrow=nrow(xaux), ncol=ncol(xaux))
   no_LOD_NA_indicator[is.na(xaux)] <- TRUE
#   no_LOD_NA_indicator <- sweep(xaux, 1, lod, "<")
#   no_LOD_NA_indicator[is.na(no_LOD_NA_indicator)] <- TRUE
  }


  # Add missings due to LOD if needed:
  if(nlod > 0){
  faux <- function(x, lod){
    x[x < lod] <- NA
    x
  }
  x[proteins_wlod, ] <- t(as.data.frame(
    base::mapply(faux,
                 x = as.list(as.data.frame(t(x[proteins_wlod,]))),
                 lod = as.list(lod[proteins_wlod]))
    ))
  }

  # Computing NA proportion for each protein:
  proteins_pNA <- apply(x[proteins,], 1, function(x) as.double(mean(is.na(x))))
  proteins_pNA <- data.frame(protein = proteins, pNA = proteins_pNA)


  # Protein's missings information:

  if(nlod == 0){
    missings_data <- proteins_pNA
    missings_data[, "pLOD/pNA"] <- NA

  } else {
    missings_data <- merge(proteins_pLOD, proteins_pNA, by = "protein", all = TRUE)
    missings_data[,2] <- round(missings_data[,2]*100,1)
    missings_data[,3] <- round(missings_data[,3]*100,1)
    aux <- ifelse(is.na(missings_data$pLOD), 0, missings_data$pLOD) / missings_data$pNA
    missings_data[, "pLOD/pNA"] <- round(as.double(ifelse(is.nan(aux), 0, aux))*100,1)
    rm(aux)
  }


  # Selected proteins to be imputed:

  if(nlod > 0){
  proteins_sel <- proteins[(proteins %in%
       as.character(missings_data$protein)[missings_data$pNA <= pNA &
       (missings_data$`pLOD/pNA` >= pLOD | is.na(missings_data$`pLOD/pNA`))]) &
      (!proteins %in% proteins_to_exclude)]
  } else {
      proteins_sel <- proteins[(proteins %in%
        as.character(missings_data$protein)[missings_data$pNA <= pNA]) &
          (!proteins %in% proteins_to_exclude)]
  }

  missings_data$action <- ifelse(missings_data$protein %in% proteins_sel, "Imputed", "No-imputed")


  # Imputing filtered proteins:

  if(log) {
    # Normalized variables if required:
    logg <- function(x) ifelse(x == 0, log(x + diff(range(x, na.rm = TRUE)) / 1e5), log(x))
    x_sel <- as.data.frame(t(apply(x[proteins_sel,], 1, logg)))
  } else{
    x_sel <- x[proteins_sel, ]
  }
  set.seed(seed)

  x_imp <- imputeLCMD::impute.QRILC(x_sel)[[1]]


  if(log){
  # Inverse log-transformation if required:
    x_imp <- apply(x_imp, 2, exp)
  }
  x_imp <- rbind(x_imp, x[!rnames %in% proteins_sel,])
  x_imp <- x_imp[order(rownames(x_imp)),]

## How to x_imp[no_LOD_NA_indicator] <- NA only if lod information and if required:
#  if(nlod > 0 & onlyLODmissings & !is.null(no_LOD_NA_indicator)){
#      faux <- function(x, ind){ x[ind] <- NA; x }
#  x_imp[proteins_wlod,] <- as.data.frame(t(base::mapply(faux,
#               x = as.list(as.data.frame(t(x_imp[proteins_wlod,]))),
#               ind = as.list(as.data.frame(t(no_LOD_NA_indicator[proteins_wlod,]))))))
#  }

##  Replaced by JRG 11/Oct/2016
   x_imp[no_LOD_NA_indicator] <- NA

  missings_data <- missings_data[order(missings_data$protein),]
  colnames(missings_data)[1] <- "feature"


  res <- list(imputed.data = x_imp, missing.information = missings_data)
  res
}
