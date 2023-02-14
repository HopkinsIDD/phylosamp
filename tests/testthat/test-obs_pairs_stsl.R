test_that("obs_pairs_stsl is deprecated", {
    expect_snapshot({
        expect_equal(obs_pairs_stsl(eta = 1, chi = 1, rho = 1, M = 10), 5)
    })
})
