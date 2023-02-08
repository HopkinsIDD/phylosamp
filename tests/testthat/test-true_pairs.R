test_that("true_pairs is deprecated", {
    expect_snapshot({
        expect_equal(true_pairs(eta = 1, rho = 1, M = 10, R = NULL, assumption = "stsl"),
            5)
    })
})
