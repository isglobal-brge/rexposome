#' \code{ExposomeClust} for testing purpouses
#'
#'  \code{ExposomeClust} created from an \code{ExposomeSet}
#'  with full set of 104 exposures, 1200 simulated samples
#'  and 4 phenotypes (asthma status, BMI measure, sex, and age).
#'  The clustering was done using \code{hclust} and
#'  \code{cutree} with \code{k = 3}.
#'
#' @usage data("eclust")
#' @return An \code{ExposomeSet} object.
#' @examples
#' data("eclust")
#' dim(expo_c)
#' table(classification(expo_c))
"expo_c"
