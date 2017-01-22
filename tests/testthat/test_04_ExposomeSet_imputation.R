library(testthat)
suppressMessages(library(rexposome))

context("ExposomeSet - Imputation Methods")

create_exposome <- function(n=500) {
    data(exposome, package="rexposome")
    return(expo[ , 1:n])
}

## ------------------------------------------------------------------------- ##
test_that("Test 'tableMissing' method.", {
    x <- create_exposome()

    expect_equal(sum(tableMissings(x, set="phenotypes")), 0)
    expect_equal(sum(tableMissings(x, set="exposures")), 54)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'tableLOD' method.", {
    x <- create_exposome()

    expect_equal(sum(tableLOD(x)), 336)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'impute' method with 'mice' system.", {
  x <- create_exposome()

  x.i <- impute(x)
  expect_equal(sum(tableMissings(x.i, set="exposures")), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'impute' method with 'hmisc' system.", {
    x <- create_exposome()

    x.i <- impute(x, ssystem="hmisc")
    expect_equal(sum(tableMissings(x.i, set="exposures")), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'ilod' method with 'QRILC'.", {
    x <- create_exposome(n=1200)

    x.i <- ilod(x, method="QRILC", pN=0.8)
    expect_equal(sum(tableLOD(x.i)), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'ilod' method with 'MinProb'.", {
    x <- create_exposome(n=1200)

    x.i <- ilod(x, method="MinProb", pN=0.8, q=0)
    expect_equal(sum(tableLOD(x.i)), 3)
})
