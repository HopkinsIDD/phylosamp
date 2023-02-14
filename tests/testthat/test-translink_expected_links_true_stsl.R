
test_that("translink_expected_links_true_stsl returns (M/2) * rho when sensitivity perfect", {

    expect_equal(translink_expected_links_true_stsl(sensitivity = 1, rho = 1, M = 10),
        5)

    expect_equal(translink_expected_links_true_stsl(sensitivity = 1, rho = 0.5, M = 10),
        2.5)
})

test_that("translink_expected_links_true_stsl returns 0 when sensitivity 0", {

    expect_equal(translink_expected_links_true_stsl(sensitivity = 0, rho = 1, M = 10),
        0)

})


test_that("translink_expected_links_true_stsl fails when parameters invalid", {

    expect_error(translink_expected_links_true_stsl(sensitivity = 5, rho = 1, M = 10))

    expect_error(translink_expected_links_true_stsl(sensitivity = 1, rho = 5, M = 10))

    expect_error(translink_expected_links_true_stsl(sensitivity = 1, rho = 0.5, M = -1))

    expect_error(translink_expected_links_true_stsl(sensitivity = 0.99, rho = 0,
        M = 10))
})
