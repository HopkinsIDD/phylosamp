test_that("true_pairs_stsl is deprecated", {
    expect_snapshot({
        expect_equal(true_pairs_stsl(eta = 1, rho = 1, M = 10), 5)
    })
})
