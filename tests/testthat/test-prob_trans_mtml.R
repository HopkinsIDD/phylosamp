test_that("prob_trans_mtml is deprecated", {
    expect_snapshot({
        expect_equal(prob_trans_mtml(eta = 1, chi = 1, rho = 1, M = 10, R = 1), 1)
    })
})
