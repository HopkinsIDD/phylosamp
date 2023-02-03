test_that("gen_dists is deprecated", {
    expect_snapshot({
        expect_equal(gen_dists(mut_rate = 1, mean_gens_pdf = c(0.2, 0.2, 0.2, 0.2,
            0.2), max_dist = 0)[, "linked_prob"][[1]], 1)
    })
})
