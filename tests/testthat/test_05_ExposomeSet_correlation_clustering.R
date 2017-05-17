library(testthat)
suppressMessages({
    library(rexposome)
    #library(flexmix)
})

context("ExposomeSet - compute correlation, compute clustering and compute PCA")

create_exposome <- function(n=104, m=1200) {
    data(exposome, package="rexposome")
    return(expo[1:n, 1:m])
}

# flexmix_clust <- function(data, ...) {
#     t <- as.formula(paste0(paste0(colnames(data), collapse="+"), "~1"))
#     flexmix(t, data, ...)
# }
#
# flexmix_clas <- function(model, ...) {
#     return(clusters(model))
# }

## ------------------------------------------------------------------------- ##
test_that("Test 'correlation' method.", {
    x <- create_exposome()

    expect_error({
        c <- correlation(x, method="pearson")
    })

    expect_warning({
        c <- correlation(x, method.cor="pearson", use="pairwise.complete.obs")
    })
})

# ## ------------------------------------------------------------------------- ##
# test_that("Test 'clustering' method.", {
#     x <- create_exposome(n=25, m=100)
#
#     x <- impute(x)
#     expect_warning({
#         c <- clustering(x,
#             method=flexmix_clust,
#             cmethod=flexmix_clas,
#             k=4
#         )
#     })
# })


## ------------------------------------------------------------------------- ##
test_that("Test 'pca' method.", {
    x <- create_exposome()

    expect_warning({
        p <- pca(x)
    })
})
