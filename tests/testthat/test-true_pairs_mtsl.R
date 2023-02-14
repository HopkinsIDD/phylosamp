test_that("true_pairs_mtsl is deprecated", {
    expect_snapshot({
        expect_equal(true_pairs_mtsl(eta = 1, rho = 1, M = 10, R = 1), 10)
    })
})
