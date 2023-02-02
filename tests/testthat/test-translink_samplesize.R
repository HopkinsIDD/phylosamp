
test_that("fails when sensitivity 0", {

    expect_error(translink_samplesize(sensitivity = 0, specificity = 0.995, N = 100,
        R = 1, tdr = 0.75))

})


test_that("fails when parameters invalid", {

    expect_error(translink_samplesize(sensitivity = 0.99, specificity = 0.995, N = 100,
        R = 1, tdr = 2))

    expect_error(translink_samplesize(sensitivity = 0.99, specificity = 0.995, N = 100,
        R = 1, tdr = -2))
})
