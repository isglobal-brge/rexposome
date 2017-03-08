library(testthat)
suppressMessages(library(rexposome))

context("ExposomeSet - Basics Methods")

create_exposome <- function() {
    path <- paste0(path.package("rexposome"), .Platform$file.sep, "extdata")
    if(path == paste0(path.expand("~"), "/Projects/rexposome/extdata")) {
        path <- paste0(path.expand("~"), "/Projects/rexposome/inst/extdata/")
    }
    description <- paste0(path, .Platform$file.sep, "exposFam.txt")
    phenotype <- paste0(path, .Platform$file.sep, "phenoData.txt")
    exposures <- paste0(path, .Platform$file.sep, "exposome.txt")
    read_exposome(exposures, description, phenotype, sep = "\t")
}

## ------------------------------------------------------------------------- ##
test_that("Test 'read_exposome' function.", {
    x <- create_exposome()

    expect_true(isS4(x), info="'x' is not a S4 object.")
    expect_is(x, "ExposomeSet", info="'x' is not an 'ExposomeSet'.")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'Summary' method.", {
    x <- create_exposome()

    expect_equal(ncol(Summary(x, "phenotypes")), 4, info="Summary for 'phenotype' must have 4 columns")
    expect_equal(ncol(Summary(x, "exposures")), 104, info="Summary for 'exposures' must have 104 columns")
})

## ------------------------------------------------------------------------- ##
test_that("Test '[' operator.", {
    x <- create_exposome()

    x1 <- x[1:3, ]
    x2 <- x[ , 1:3]

    expect_equal(length(exposureNames(x1)), 3)
    expect_equal(length(sampleNames(x2)), 3)

    expect_equal(length(phenotypeNames(x1)), 4)
    expect_equal(length(phenotypeNames(x2)), 4)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'expsureNames' method.", {
    x <- create_exposome()

    expect_is(exposureNames(x), "character")
    expect_equal(length(exposureNames(x)), 104)
    expect_equal(exposureNames(x)[1], "Ben_preg")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'sampleNames' method.", {
    x <- create_exposome()

    expect_is(sampleNames(x), "character")
    expect_equal(length(sampleNames(x)), 1200)
    expect_equal(sampleNames(x)[1], "SIMSAB1000")
})

## ------------------------------------------------------------------------- ##
# test_that("Test 'individualNames' method.", {
#     x <- create_exposome()
#
#     expect_is(individualNames(x), "character")
#     expect_equal(length(individualNames(x)), 1200)
#     expect_equal(individualNames(x)[1], "SIMSAB1000")
# })

## ------------------------------------------------------------------------- ##
test_that("Test 'phenotypeNames' method.", {
    x <- create_exposome()

    expect_is(phenotypeNames(x), "character")
    expect_equal(length(phenotypeNames(x)), 4)
    expect_equal(phenotypeNames(x)[1], "asthma")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'familyNames' method.", {
    x <- create_exposome()

    expect_is(familyNames(x), "character")
    expect_equal(length(familyNames(x)), 12)
    expect_equal(familyNames(x)[1], "Ambient air pollutants")
})


## ------------------------------------------------------------------------- ##
# test_that("Test 'as.data.frame' method.", {
#     x <- create_exposome()
#     t <- as.data.frame(x)
#
#     expect_is(t, "data.frame")
#     expect_equal(ncol(t), 108)
#     expect_equal(nrow(t), 1200)
#
#     t <- as.data.frame(x, phe=FALSE)
#
#     expect_equal(ncol(t), 104)
# })

## ------------------------------------------------------------------------- ##
test_that("Test 'expos' method.", {
    x <- create_exposome()
    t <- expos(x)

    expect_is(t, "data.frame")
    expect_equal(ncol(t), 104)
    expect_equal(nrow(t), 1200)
})
