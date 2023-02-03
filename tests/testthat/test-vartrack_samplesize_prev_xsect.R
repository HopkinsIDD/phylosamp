
test_that("input arguments are valid", {

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0, prob = 0.4, omega = 0.9,
        precision = 0.05, c_ratio = 1))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 1.1, prob = 0.9, omega = 0.9,
        precision = 0.05, c_ratio = 1))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = Inf, prob = 0.7, omega = 0.9,
        precision = 0.05, c_ratio = 1))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = -0.2, omega = 0.9,
        precision = 0.05, c_ratio = 1.3))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 1, omega = 0.9,
        precision = 0.05, c_ratio = 1.5))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = "invalid", omega = 0.9,
        precision = 0.05, c_ratio = 0.9))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.8, omega = "invalid",
        precision = 0.05, c_ratio = 0.9))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.8, omega = -0.9,
        precision = 0.05, c_ratio = 0.9))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.8, omega = 1.9,
        precision = 0.05, c_ratio = 0.9))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.95, omega = 0.9,
        precision = 0, c_ratio = 1.3))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.95, omega = 0.9,
        precision = 1.2, c_ratio = 1.5))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.95, omega = 0.9,
        precision = "invalid", c_ratio = 0.9))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.1, prob = 0.3, omega = 0.9,
        precision = 0.05, c_ratio = 0))

    expect_error(vartrack_samplesize_prev_xsect(p_v1 = 0.1, prob = 0.2, omega = 0.9,
        precision = 0.05, c_ratio = "invalid"))
})

test_that("return object is valid double", {

    expect_type(vartrack_samplesize_prev_xsect(p_v1 = 0.2, prob = 0.9, precision = 0.02,
        omega = 0.9, c_ratio = 1.5), "double")
})

test_that("manuscript results remain valid", {

    expect_equal(ceiling(vartrack_samplesize_prev_xsect(p_v1 = 0.1, prob = 0.95,
        precision = 0.25, omega = 1, c_ratio = 1)), 554)

    expect_equal(ceiling(vartrack_samplesize_prev_xsect(p_v1 = 0.1, prob = 0.95,
        precision = 0.25, omega = 1, c_ratio = 1/0.84)), 465)
})
