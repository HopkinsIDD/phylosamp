
test_that("translink_expected_links_obs_mtml returns M*rho when sensitivity perfect", {

    expect_equal(translink_expected_links_obs_mtml(sensitivity = 1, specificity = 1,
        rho = 1, M = 10, R = 1), 10)

    expect_equal(translink_expected_links_obs_mtml(sensitivity = 1, specificity = 1,
        rho = 0.5, M = 10, R = 1), 5)
})

test_that("translink_expected_links_obs_mtml fails when parameters invalid", {

    expect_error(translink_expected_links_obs_mtml(sensitivity = 5, specificity = 1,
        rho = 1, M = 10, R = 1))

    expect_error(translink_expected_links_obs_mtml(sensitivity = 1, specificity = 5,
        rho = 0.5, M = 10, R = 1))

    expect_error(translink_expected_links_obs_mtml(sensitivity = 1, specificity = 1,
        rho = 5, M = 10, R = 1))

    expect_error(translink_expected_links_obs_mtml(sensitivity = 1, specificity = 1,
        rho = 0.5, M = -1, R = 1))

    expect_error(translink_expected_links_obs_mtml(sensitivity = 0.99, specificity = 0.99,
        rho = 0, M = 10, R = 1))

    expect_warning(translink_expected_links_obs_mtml(sensitivity = 0.99, specificity = 0.99,
        rho = 0.5, M = 10, R = 2))
})
