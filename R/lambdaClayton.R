#' Lambda Calculation for a vector of P-Values
#'
#' Implementation of Clayton's lambda score for a vector of P-Values
#'
#' @author Juran R. González
#' @export
lambdaClayton <- function(x, trim=0.5) {
    xx <- qnorm(1-x)^2
    N <- length(xx)
    obsvd <- sort(xx, na.last=NA)
    expctd <- qchisq(p = (1:N)/(N + 1), 1)
    Nu <- floor(trim * N)
    lambda <- mean(obsvd[1:Nu])/mean(expctd[1:Nu])
    lambda
}
