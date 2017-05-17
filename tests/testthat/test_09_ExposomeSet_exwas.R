library(testthat)
suppressMessages({
    library(rexposome)
})

context("ExposomeSet - exwas")

create_exposome <- function(n=104, m=1200) {
    data(exposome, package="rexposome")
    return(expo[1:n , 1:m])
}

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - binomial phenotype, raw model.", {
    x <- create_exposome()

    expect_warning({
        tt <- exwas(x, asthma~1, family="binomial")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - binomial phneotype, sex adjusted model.", {
    x <- create_exposome()

    expect_warning({
        tt <- exwas(x, asthma~sex, family="binomial")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 52)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - binomial phneotype, sex adjusted model, filtered by flu.", {
    x <- create_exposome()

    expect_warning({
        tt <- exwas(x, asthma~sex, filter=age==4, family="binomial")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 50)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 5)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - gaussian phneotype, raw model.", {
    x <- create_exposome(n=5)

    expect_warning({
        tt <- exwas(x, age~1, family="gaussian")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 2)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - gaussian phneotype, sex adjusted model.", {
    x <- create_exposome(n=5)

    expect_warning({
        tt <- exwas(x, age~sex, family="gaussian")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 2)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - gaussian phneotype, sex adjusted model, filtered by flu.", {
    x <- create_exposome(n=5)

    expect_warning({
        tt <- exwas(x, age~sex, filter=asthma=="asthma", family="gaussian")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 3)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'exwas' - gaussian phneotype previous subsept by exposures.", {
    x <- create_exposome(n=15)

    sel <- rownames(fData(x))[fData(x)$Family == "Organochlorines"]

    expect_warning({
        tt <- exwas(x[sel, ], age~sex, filter=asthma=="asthma", family="gaussian")
    })
    expect_is(tt, "ExWAS")

    expect_equal(sum(extract(tt)$pvalue < 0.5), 6)
    expect_equal(sum(extract(tt)$pvalue < 0.05), 0)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotEffect' - Single ExWAS.", {
    x <- create_exposome(n=15)

    sel <- rownames(fData(x))[fData(x)$Family == "Organochlorines"]

    expect_warning({
        tt <- exwas(x[sel, ], age~sex, filter=asthma=="asthma", family="gaussian")
    })
    expect_is(tt, "ExWAS")

    plotEffect(tt)
})

## ------------------------------------------------------------------------- ##
test_that("Test 'plotEffect' - Dual ExWAS.", {
    x <- create_exposome()

    expect_warning({
        t1 <- exwas(x, age~sex, filter=asthma=="asthma", family="gaussian")
        t2 <- exwas(x, age~1, filter=asthma=="asthma", family="gaussian")
    })
    expect_is(t1, "ExWAS")
    expect_is(t2, "ExWAS")

    plotEffect(x = t1, y = t2)
})
