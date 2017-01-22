library(testthat)
suppressMessages({
    library(rexposome)
    library(flexmix)
})

context("ExposomePCA - Inner Methods")

create_exposome_pca <- function(n=50, m=500) {
    data(exposome, package="rexposome")
    xx <- impute(expo[1:n, 1:m])
    pca(xx)
}

## ------------------------------------------------------------------------- ##
test_that("Test 'pca' method from 'ExposomeSet'.", {
    expect_warning({
        p <- create_exposome_pca()
    })

    expect_equal(ndim(p), 10)
})
