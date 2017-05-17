library(testthat)
suppressMessages({
    library(rexposome)
})

context("ExposomeCorr - Inner Methods")

create_exposome_corr <- function(n=104, m=1200) {
    data(exposome, package="rexposome")
    correlation(expo[1:n, 1:m], method.cor="pearson", use="pairwise.complete.obs")
}

## ------------------------------------------------------------------------- ##
test_that("Test 'correlation' method from 'ExposomeSet'.", {
    expect_warning({
        c <- create_exposome_corr()
    })
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotCorrelation'.", {
    expect_warning({
        c <- create_exposome_corr()
    })

    plotCorrelation(c)
    plotCorrelation(c, type="matrix")
    plotCorrelation(c, type="circos")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'correlation' method from 'ExposomeSet'.", {
    expect_warning({
        c <- create_exposome_corr()
    })

    tt <- extract(c)

    expect_equal(ncol(tt), 104)
    expect_equal(nrow(tt), 104)
})
