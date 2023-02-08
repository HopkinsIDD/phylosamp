
test_that("translink_prob_transmit_mtml returns 1 when sensitivity perfect", {

    expect_equal(translink_prob_transmit_mtml(sensitivity = 1, specificity = 1, rho = 1,
        M = 10, R = 1), 1)

    expect_equal(translink_prob_transmit_mtml(sensitivity = 1, specificity = 1, rho = 0.5,
        M = 10, R = 1), 1)
})

test_that("translink_prob_transmit_mtml returns 0 when sensitivity 0", {

    expect_equal(translink_prob_transmit_mtml(sensitivity = 0, specificity = 0.99,
        rho = 1, M = 10, R = 1), 0)

    expect_equal(translink_prob_transmit_mtml(sensitivity = 0, specificity = 0.99,
        rho = 0.5, M = 10, R = 1), 0)

    expect_equal(translink_prob_transmit_mtml(sensitivity = 0, specificity = 0.5,
        rho = 1, M = 10, R = 1), 0)
})

test_that("translink_prob_transmit_mtml fails when parameters invalid", {

    expect_error(translink_prob_transmit_mtml(sensitivity = 5, specificity = 1, rho = 1,
        M = 10, R = 1))

    expect_error(translink_prob_transmit_mtml(sensitivity = 1, specificity = 5, rho = 0.5,
        M = 10, R = 1))

    expect_error(translink_prob_transmit_mtml(sensitivity = 1, specificity = 1, rho = 5,
        M = 10, R = 1))

    expect_error(translink_prob_transmit_mtml(sensitivity = 1, specificity = 1, rho = 0.5,
        M = -1, R = 1))

    expect_error(translink_prob_transmit_mtml(sensitivity = 0.99, specificity = 0.99,
        rho = 0, M = 10, R = 1))

    expect_warning(translink_prob_transmit_mtml(sensitivity = 0.99, specificity = 0.99,
        rho = 0.5, M = 10, R = 2))
})
