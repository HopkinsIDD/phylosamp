test_that("vartrack_prob_detect_xsect input arguments are valid", {

    expect_error(vartrack_prob_detect_xsect(p_v1 = 0, n = 30, omega = 0.9, c_ratio = 1.5))
    expect_error(vartrack_prob_detect_xsect(p_v1 = "invalid", n = 30, omega = 0.9, c_ratio = 1.5))

    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 0, omega = 0.9, c_ratio = 1.5))
    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = "invalid", omega = 0.9, c_ratio = 1.5))

    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 30, omega = 0, c_ratio = 1.5))
    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 30, omega = 1.9, c_ratio = 1.5))

    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 30, omega = 0.9, c_ratio = 0))
    expect_error(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 30, omega = 0.9, c_ratio = "invalid"))
})

test_that("vartrack_prob_detect_xsect return object is valid double", {

    expect_type(vartrack_prob_detect_xsect(p_v1 = 1/10000, n = 30, omega = 0.9, c_ratio = 1.5), "double")
})


test_that("vartrack_prob_detect_xsect manuscript results remain valid", {

    expect_gt(vartrack_prob_detect_xsect(p_v1 = .02, n = 149, omega = 1, c_ratio = 1), 0.95)

    expect_gt(vartrack_prob_detect_xsect(p_v1 = .02, n = 137, omega = 0.8, c_ratio = (0.8 * 0.975)/(0.6 * 0.95)), 0.95)
})
