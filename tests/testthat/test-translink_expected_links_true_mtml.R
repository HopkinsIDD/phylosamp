
test_that("returns M*rho when sensitivity perfect", {

    expect_equal(translink_expected_links_true_mtml(sensitivity = 1, rho = 1, M = 10,
        R = 1), 10)

    expect_equal(translink_expected_links_true_mtml(sensitivity = 1, rho = 0.5, M = 10,
        R = 1), 5)
})

test_that("returns 0 when sensitivity 0", {

    expect_equal(translink_expected_links_true_mtml(sensitivity = 0, rho = 1, M = 10,
        R = 1), 0)

})

test_that("fails when parameters invalid", {

    expect_error(translink_expected_links_true_mtml(sensitivity = 5, rho = 1, M = 10,
        R = 1))

    expect_error(translink_expected_links_true_mtml(sensitivity = 1, rho = 5, M = 10,
        R = 1))

    expect_error(translink_expected_links_true_mtml(sensitivity = 1, rho = 0.5, M = -1,
        R = 1))

    expect_error(translink_expected_links_true_mtml(sensitivity = 0.99, rho = 0,
        M = 10, R = 1))

    expect_warning(translink_expected_links_true_mtml(sensitivity = 0.99, rho = 0.5,
        M = 10, R = 2))
})
