
test_that("translink_expected_links_obs returns M*rho or M/2 * rho when sensitivity perfect", {

    expect_equal(translink_expected_links_obs(sensitivity = 1, specificity = 1, rho = 1,
        M = 10, R = NULL, assumption = "stsl"), 5)

    expect_equal(translink_expected_links_obs(sensitivity = 1, specificity = 1, rho = 1,
        M = 10, R = 1, assumption = "mtsl"), 10)

    expect_equal(translink_expected_links_obs(sensitivity = 1, specificity = 1, rho = 1,
        M = 10, R = 1, assumption = "mtml"), 10)
})

test_that("translink_expected_links_obs fails when parameters invalid", {

    expect_error(translink_expected_links_obs(sensitivity = 1, specificity = 1, rho = 1,
        M = 10, R = 1, assumption = "bananas"))

})
