
test_that("gendist_roc_format finds sensitivity estimate", {

    expect_equal(gendist_roc_format(cutoff = 2, mut_rate = 1, mean_gens_pdf = c(0.02,
        0.08, 0.15, 0.75), max_link_gens = 1)[2, "sensitivity"][[1]], 0.7357589,
        tolerance = 0.01)
})

test_that("gendist_roc_format finds specificity estimate", {

    expect_equal(gendist_roc_format(cutoff = 2, mut_rate = 1, mean_gens_pdf = c(0.02,
        0.08, 0.15, 0.75), max_link_gens = 1)[2, "specificity"][[1]], 0.1337106,
        tolerance = 0.01)
})

test_that("gendist_roc_format returns sensitivity and specificity estimates for a range of cutoff values",
    {

        tmp <- gendist_roc_format(cutoff = 10, mut_rate = 1, mean_gens_pdf = c(0.02,
            0.08, 0.15, 0.75), max_link_gens = 1)

        expect_identical(as.integer(c(3, 3)), dim(tmp))
        expect_identical(as.numeric(tmp[1, c("sensitivity", "specificity")]), c(0,
            0))
        expect_identical(as.numeric(tmp[3, c("sensitivity", "specificity")]), c(1,
            1))
        expect_equal(as.numeric(tmp[2, "sensitivity"]), 0.999, tolerance = 0.01)
    })
