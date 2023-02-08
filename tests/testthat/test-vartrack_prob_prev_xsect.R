test_that("vartrack_prob_prev_xsect input arguments are valid", {

    expect_error(vartrack_prob_prev_xsect(p_v1 = 0, n = 30, precision = 0.1, omega = 0.9, c_ratio = 1.5))
    expect_error(vartrack_prob_prev_xsect(p_v1 = "invalid", n = 30, precision = 0.1, omega = 0.9, c_ratio = 1.5))

    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 0, precision = 0.1, omega = 0.9, c_ratio = 1.5))
    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = "invalid", precision = 0.1, omega = 0.9, c_ratio = 1.5))

    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 1.1, omega = 0.9, c_ratio = 1.5))
    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = "invalid", omega = 0.9, c_ratio = 1.5))

    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 0.1, omega = 0, c_ratio = 1.5))
    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 0.1, omega = 1.9, c_ratio = 1.5))

    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 0.1, omega = 0.9, c_ratio = 0))
    expect_error(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 0.1, omega = 0.9, c_ratio = "invalid"))
})

test_that("vartrack_prob_prev_xsect return object is valid double", {

    expect_type(vartrack_prob_prev_xsect(p_v1 = 1/10000, n = 30, precision = 0.1, omega = 0.9, c_ratio = 1.5), "double")
})


test_that("vartrack_prob_prev_xsect manuscript results remain valid", {

    expect_gt(vartrack_prob_prev_xsect(p_v1 = .1, n = 554, precision = 0.25, omega = 1, c_ratio = 1), 0.95)

    expect_gt(vartrack_prob_prev_xsect(p_v1 = .1, n = 465, precision = 0.25, omega = 1, c_ratio = 1/0.84), 0.95)

    expect_gt(vartrack_prob_prev_xsect(p_v1 = .1, n = 582, precision = 0.25, omega = 0.8, c_ratio = 1/0.84), 0.95)
})
