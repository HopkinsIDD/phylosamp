test_that("obs_pairs_mtsl is deprecated", {
    expect_snapshot({
        expect_equal(obs_pairs_mtsl(eta = 1, chi = 1, rho = 1, M = 10, R = 1), 10)
    })
})
