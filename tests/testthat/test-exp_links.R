test_that("exp_links is deprecated", {
    expect_snapshot({
        expect_equal(exp_links(eta = 1, chi = 1, rho = 1, M = 10, R = NULL, assumption = "stsl"),
            5)
    })
})
