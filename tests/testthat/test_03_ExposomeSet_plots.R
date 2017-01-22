library(testthat)
suppressMessages(library(rexposome))

context("ExposomeSet - Plotting Methods")

create_exposome <- function() {
    data(exposome, package="rexposome")
    expo[c(1:8, 100:104), ]
}

## ------------------------------------------------------------------------- ##
test_that("Test 'plotFamily' method.", {
    x <- create_exposome()

    plotFamily(x, family="Organochlorines")
    plotFamily(x, family="Indoor air")

    plotFamily(x, family="Organochlorines", group="sex")
    plotFamily(x, family="Indoor air", group="sex")

    plotFamily(x, family="Indoor air", group2="sex")
    plotFamily(x, family="Indoor air", group="sex", group2="asthma")
    plotFamily(x, family="Organochlorines", group="sex", group2="asthma")

    expect_warning({
        plotFamily(x, family="all")
    })
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotHistogram' method.", {
    x <- create_exposome()

    plotHistogram(x, exposure="ldde_lip")
    plotHistogram(x, exposure="ldde_lip", show.trans=TRUE)
    plotHistogram(x, exposure="ETS")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotMissings' method.", {
    x <- create_exposome()

    plotMissings(x, set="phenotype")
    plotMissings(x, set="exposures")
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotLOD' method.", {
    x <- create_exposome()

    plotLOD(expo, lod.col="LOD")
})
