test_that("vartrack_prob_detect input arguments are valid", {

    expect_error(vartrack_prob_detect(p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "fake"),
        "sampling frequency argument")
    expect_error(vartrack_prob_detect(n = 30, t = 2, omega = 1, p0_v1 = 0.1, r_v1 = .02, c_ratio = 1, sampling_freq = "xsect"),
        "the xsect method")
    expect_error(vartrack_prob_detect(p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "cont"),
        "the cont method")
    expect_error(vartrack_prob_detect(t = 2, p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "cont"),
        "the cont method")
})

test_that("vartrack_prob_detect return object is valid double", {

    expect_type(vartrack_prob_detect(p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "xsect"), "double")
    expect_type(vartrack_prob_detect(n = 30, t = 2, omega = 1, p0_v1 = 0.1, r_v1 = .02, c_ratio = 1, sampling_freq = "cont"), "double")
    expect_type(vartrack_prob_detect(n = 30, p_v1 = 0.11, omega = 1, p0_v1 = 0.1, r_v1 = .02, c_ratio = 1, sampling_freq = "cont"), "double")
})

