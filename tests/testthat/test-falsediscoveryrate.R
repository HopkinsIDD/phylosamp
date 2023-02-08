test_that("falsediscoveryrate is deprecated", {
    expect_snapshot({
        expect_equal(falsediscoveryrate(eta = 1, chi = 1, rho = 1, M = 10, R = NULL,
            assumption = "stsl"), 0)
    })
})
