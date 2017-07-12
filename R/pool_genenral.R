pool_glm <- function (object, method = "smallsample", ex) {
    expandvcov <- function(q, u) {
        err <- is.na(q)
        return(u)
    }

    mice.df <- function(m, lambda, dfcom, method) {
        if (is.null(dfcom)) {
            dfcom <- 999999
            warning("Large sample assumed.")
        }
        lambda[lambda < 1e-04] <- 1e-04
        dfold <- (m - 1)/lambda^2
        dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
        df <- dfold * dfobs/(dfold + dfobs)
        if (method != "smallsample")
            df <- dfold  ## Rubin 1987, 3.1.6, Van Buuren 2012, 2.30, added 31/10/2012
        return(df)
    }

    m <- length(object$analyses)
    fa <- mice::getfit(object, 1)
    analyses <- mice::getfit(object)

    mxv <- sapply(1:m, function(ii) { length(stats::coef(analyses[[ii]])) })
    mx <- max(mxv)

    if(sum(mxv == mx) != length(object$analyses)) {
        warning("Only ", sum(mxv == mx), " of ", length(object$analyses),
                " will be used in this analysis (", ex, ").")
    }

    k <- length(stats::coef(fa))
    names <- names(stats::coef(fa))
    qhat <- matrix(NA, nrow = m, ncol = k,
                   dimnames = list(1:m, names))
    u <- array(NA, dim = c(m, k, k),
               dimnames = list(1:m, names, names))

    for (i in 1:m) {
        fit <- analyses[[i]]
        if(length(stats::coef(fit)) == mx) {
            qhat[i, ] <- stats::coef(fit)
            ui <- vcov(fit)
            ui <- expandvcov(qhat[i, ], ui)
            if (ncol(ui) != ncol(qhat))
                stop("Different number of parameters: coef(fit): ",
                     ncol(qhat), ", vcov(fit): ", ncol(ui), "(", ex, ")")
            u[i, , ] <- array(ui, dim = c(1, dim(ui)))
        }
    }

    qhat <- qhat[mxv == mx, ]
    u <- u[mxv == mx, , ]
    m <- sum(mxv == mx)

    # qbar: final coefficients
    qbar <- apply(qhat, 2, mean)
    ubar <- apply(u, c(2, 3), mean)
    e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    b <- (t(e) %*% e)/(m - 1)
    # t: final covariance matrix of coefficients
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * diag(b/ubar)
    lambda <- (1 + 1/m) * diag(b/t)
    dfcom <- stats::df.residual(fa)

    df <- mice.df(m, lambda, dfcom, method)
    fmi <- (r + 2/(df + 3))/(r + 1)

    # only one of the coefficients is the coef of interest
    # e.g. it is the 2nd one
    j <- 2

    tval <- qbar[j]/sqrt(t[j,j])
    pval <- 2 * pt(abs(tval), df[j], lower.tail = FALSE)

    names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
    fit <- list(call = call, call1 = object$call, call2 = object$call1,
                nmis = object$nmis, m = m, qhat = qhat, u = u, qbar = qbar,
                ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
                fmi = fmi, lambda = lambda, pval = pval)
    oldClass(fit) <- c("mipo", oldClass(object))
    return(fit)
}
