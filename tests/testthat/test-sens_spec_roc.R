test_that("sens_spec_roc is deprecated", {
    expect_snapshot({
        expect_equal(sens_spec_roc(cutoff = 2, mut_rate = 1, mean_gens_pdf = c(0.02,
            0.08, 0.15, 0.75), max_link_gens = 1)[2, "sensitivity"][[1]], 0.7357589,
            tolerance = 0.01)
    })
})
