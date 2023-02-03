test_that("prob_trans_stsl is deprecated", {
    expect_snapshot({
        expect_equal(prob_trans_stsl(eta = 1, chi = 1, rho = 1, M = 10), 1)
    })
})
