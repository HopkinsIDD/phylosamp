test_that("sampleize is deprecated", {
    expect_snapshot({
        expect_error(samplesize(eta = 0, chi = 0.995, N = 100, R = 1, phi = 0.75))
    })
})
