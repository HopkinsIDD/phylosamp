test_that("truediscoveryrate is deprecated", {
    expect_snapshot({
        expect_equal(truediscoveryrate(eta = 1, chi = 1, rho = 1, M = 10, R = NULL,
            assumption = "stsl"), 1)
    })
})
