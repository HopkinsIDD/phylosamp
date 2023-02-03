test_that("obs_pairs_mtml is deprecated", {
    expect_snapshot({
        expect_equal(obs_pairs_mtml(eta = 1, chi = 1, rho = 1, M = 10, R = 1), 10)
    })
})
