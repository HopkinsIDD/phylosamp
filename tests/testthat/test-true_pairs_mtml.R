test_that("true_pairs_mtml is deprecated", {
    expect_snapshot({
        expect_equal(true_pairs_mtml(eta = 1, rho = 1, M = 10, R = 1), 10)
    })
})
