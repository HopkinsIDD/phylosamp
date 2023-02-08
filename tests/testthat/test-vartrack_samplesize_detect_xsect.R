
test_that("vartrack_samplesize_detect_xsect input arguments are valid", {

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0, prob = 0.4, omega = 0.9,
        c_ratio = 1))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 1.1, prob = 0.9, omega = 0.9,
        c_ratio = 1))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = Inf, prob = 0.7, omega = 0.9,
        c_ratio = 1))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = -0.2, omega = 0.9,
        c_ratio = 1.3))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = 1, omega = 0.9,
        c_ratio = 1.5))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = "invalid", omega = 0.9,
        c_ratio = 0.9))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = 0.8, omega = "invalid",
        c_ratio = 0.9))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = 0.8, omega = -0.3,
        c_ratio = 0.9))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = 0.8, omega = 1.5,
        c_ratio = 0.9))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.1, prob = 0.3, omega = 0.9,
        c_ratio = -0.9))

    expect_error(vartrack_samplesize_detect_xsect(p_v1 = 0.1, prob = 0.2, omega = 0.9,
        c_ratio = "invalid"))
})

test_that("vartrack_samplesize_detect_xsect return object is valid double", {

    expect_type(vartrack_samplesize_detect_xsect(p_v1 = 0.2, prob = 0.9, omega = 0.9,
        c_ratio = 1.5), "double")

})

test_that("vartrack_samplesize_detect_xsect manuscript results remain valid", {

    expect_equal(ceiling(vartrack_samplesize_detect_xsect(p_v1 = 0.02, prob = 0.95,
        omega = 1, c_ratio = 1)), 149)

    expect_equal(ceiling(vartrack_samplesize_detect_xsect(p_v1 = 0.02, prob = 0.95,
        omega = 1, c_ratio = (0.8 * 0.975)/(0.6 * 0.95))), 109)
})
