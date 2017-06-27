pool_glm <- function (analyses, m, method = "smallsample") {
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

    # fa <- mice::getfit(object, 1)
    # analyses <- mice::getfit(object)

    k <- length(coef(analyses[[1]]))
    names <- names(coef(analyses[[1]]))

    # k <- length(stats::coef(fa))
    # names <- names(stats::coef(fa))
    qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m,
                                                           names))
    u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names,
                                                     names))
    for (ii in 1:m) {
        fit <- analyses[[ii]]
        qhat[ii, ] <- stats::coef(fit)
        ui <- vcov(fit)
        ui <- expandvcov(qhat[ii, ], ui)
        if (ncol(ui) != ncol(qhat))
            stop("Different number of parameters: coef(fit): ",
                 ncol(qhat), ", vcov(fit): ", ncol(ui))
        u[ii, , ] <- array(ui, dim = c(1, dim(ui)))
    }

    # qbar: final coefficients
    qbar <- apply(qhat, 2, mean)
    ubar <- apply(u, c(2, 3), mean)
    e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    b <- (t(e) %*% e)/(m - 1)
    # t: final covariance matrix of coefficients
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * diag(b/ubar)
    lambda <- (1 + 1/m) * diag(b/t)
    #dfcom <- df.residual(fa)
    dfcom <- df.residual(analyses[[1]])

    df <- mice.df(m, lambda, dfcom, method)
    fmi <- (r + 2/(df + 3))/(r + 1)

    # only one of the coefficients is the coef of interest
    # e.g. it is the 2nd one
    j <- 2

    tval <- qbar[j]/sqrt(t[j,j])
    pval <- 2 * pt(abs(tval), df[j], lower.tail = FALSE)

    names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
    fit <- list(call = call, call1 = NULL, call2 = NULL,
                nmis = NULL, m = m, qhat = qhat, u = u, qbar = qbar,
                ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
                fmi = fmi, lambda = lambda, pval = pval)
    oldClass(fit) <- c("mipo", "glm")
    return(fit)



    # expandvcov <- function (q, u) {
    #     err <- is.na(q)
    #     return(u)
    # }
    # df_residual <- function (object, q = 1.3, ...) {
    #     df <- object$df.residual
    #     if (!is.null(df))
    #         return(df)
    #     mk <- try(c <- stats::coef(object), silent = TRUE)
    #     mn <- try(f <- stats::fitted(object), silent = TRUE)
    #     if (inherits(mk, "try-error") | inherits(mn, "try-error"))
    #         return(NULL)
    #     n <- ifelse(is.data.frame(f) | is.matrix(f), nrow(f), length(f))
    #     k <- length(c)
    #     if (k == 0 | n == 0)
    #         return(NULL)
    #     return(max(1, n - q * k))
    # }
    # mice_df <- function (m, lambda, dfcom, method) {
    #     if (is.null(dfcom)) {
    #         dfcom <- 999999
    #         warning("Large sample assumed.")
    #     }
    #     lambda[lambda < 1e-04] <- 1e-04
    #     dfold <- (m - 1)/lambda^2
    #     dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
    #     df <- dfold * dfobs/(dfold + dfobs)
    #     if (method != "smallsample")
    #         df <- dfold
    #     return(df)
    # }
    #
    # k <- length(coef(analyses[[1]]))
    # names <- names(coef(analyses[[1]]))
    #
    # qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m,
    #                                                        names))
    # u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names,
    #                                                  names))
    #
    # for (i in 1:m) {
    #     fit <- analyses[[i]]
    #     qhat[i, ] <- coef(fit)
    #     ui <- vcov(fit)
    #     ui <- expandvcov(qhat[i, ], ui)
    #     if (ncol(ui) != ncol(qhat))
    #         stop("Different number of parameters: coef(fit): ",
    #              ncol(qhat), ", vcov(fit): ", ncol(ui))
    #     u[i, , ] <- array(ui, dim = c(1, dim(ui)))
    # }
    # qbar <- apply(qhat, 2, mean)
    # ubar <- apply(u, c(2, 3), mean)
    # e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    # b <- (t(e) %*% e)/(m - 1)
    # t <- ubar + (1 + 1/m) * b
    # r <- (1 + 1/m) * diag(b/ubar)
    # lambda <- (1 + 1/m) * diag(b/t)
    # dfcom <- df_residual(analyses[[1]])
    # df <- mice_df(m, lambda, dfcom, method)
    # fmi <- (r + 2/(df + 3))/(r + 1)
    # names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
    # fit <- list(call = call, call1 = NA, call2 = NA, nmis = NA, m = m,
    #             qhat = qhat, u = u, qbar = qbar,
    #             ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
    #             fmi = fmi, lambda = lambda)
    # oldClass(fit) <- c("mipo", "glm")
    #
    # bb <<- b
    # return(fit)
}
