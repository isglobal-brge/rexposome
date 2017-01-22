library(testthat)
suppressMessages(library(rexposome))

context("ExposomeSet - Transformation Methods")

create_exposome <- function(both=TRUE) {
    data(exposome, package="rexposome")
    if(both) {
        expo[c(1:8, 100:104), ]
    } else {
        expo[c(1:8), ]
    }
}

## ------------------------------------------------------------------------- ##
test_that("Test 'standardize' method (factor and numeric).", {
    x <- create_exposome()

    expect_warning({
        x.n <- standardize(x, method="normal")
    })
    expect_warning({
        x.r <- standardize(x, method="robust")
    })
})

## ------------------------------------------------------------------------- ##
test_that("Test 'highAndLow' method.", {
  x <- create_exposome()

  expect_warning({
    x.d1 <- highAndLow(x, ngroups = 3)
    x.d2 <- highAndLow(x, ngroups = 3, drop=TRUE)
  })

  expect_equal(length(exposureNames(x.d1)), 13 + 8)
  expect_equal(length(exposureNames(x.d2)), 13)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'trans' method.", {
    x <- create_exposome(both=FALSE)

    x.e <- trans(trans(x, log), exp)

    expect_equal(assayDataElement(x, "exp")[1, 1],
                 assayDataElement(x.e, "exp")[1, 1])
    expect_true(identical(assayDataElement(x, "exp")[1, 1:10],
                          assayDataElement(x.e, "exp")[1, 1:10]))
})

## ------------------------------------------------------------------------- ##
test_that("Test 'normalityTest' method.", {
    x <- create_exposome(both=FALSE)

    x.t <- normalityTest(x)

    expect_equal(sum(!x.t$normality, na.rm=TRUE), 7)
    expect_equal(sum(x.t$normality, na.rm=TRUE), 1)

    x.l <- trans(x, log)
    x.t <- normalityTest(x.l)

    expect_equal(sum(x.t$normality, na.rm=TRUE), 8)
})
