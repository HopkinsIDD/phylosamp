test_that("prob_trans_mtsl is deprecated", {
    expect_snapshot({
        expect_equal(prob_trans_mtsl(eta = 1, chi = 1, rho = 1, M = 10, R = 1), 1)
    })
})
