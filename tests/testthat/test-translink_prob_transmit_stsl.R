
test_that("translink_prob_transmit_stsl returns 1 when sensitivity perfect", {

    expect_equal(translink_prob_transmit_stsl(sensitivity = 1, specificity = 1, rho = 1,
        M = 10), 1)

    expect_equal(translink_prob_transmit_stsl(sensitivity = 1, specificity = 1, rho = 0.5,
        M = 10), 1)

    expect_equal(translink_prob_transmit_stsl(sensitivity = 1, specificity = 0, rho = 1,
        M = 10), 1)

    expect_equal(translink_prob_transmit_stsl(sensitivity = 1, specificity = 0.5,
        rho = 1, M = 10), 1)
})

test_that("translink_prob_transmit_stsl returns 0 when sensitivity 0", {

    expect_equal(translink_prob_transmit_stsl(sensitivity = 0, specificity = 0, rho = 1,
        M = 10), 0)

    expect_equal(translink_prob_transmit_stsl(sensitivity = 0, specificity = 0, rho = 0.5,
        M = 10), 0)

    expect_equal(translink_prob_transmit_stsl(sensitivity = 0, specificity = 0.9,
        rho = 1, M = 10), 0)
})

test_that("translink_prob_transmit_stsl fails when parameters invalid", {

    expect_error(translink_prob_transmit_stsl(sensitivity = 5, specificity = 1, rho = 1,
        M = 10))

    expect_error(translink_prob_transmit_stsl(sensitivity = 1, specificity = 5, rho = 0.5,
        M = 10))

    expect_error(translink_prob_transmit_stsl(sensitivity = 1, specificity = 1, rho = 5,
        M = 10))

    expect_error(translink_prob_transmit_stsl(sensitivity = 1, specificity = 1, rho = 0.5,
        M = -1))

    expect_error(translink_prob_transmit_stsl(sensitivity = 1, specificity = 1, rho = 0,
        M = 10))
})
