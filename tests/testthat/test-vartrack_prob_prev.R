test_that("vartrack_prob_prev input arguments are valid", {

    expect_error(vartrack_prob_prev(p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "fake"),
        "sampling frequency argument")
    expect_error(vartrack_prob_prev(n = 30, c_ratio = 1, sampling_freq = "xsect"),
        "missing")
    expect_error(vartrack_prob_prev(p_v1 = 0.1, n = 30, omega = 0.9, c_ratio = 1.5, sampling_freq = "cont"),
        "not yet implemented")
})

test_that("vartrack_prob_prev return object is valid double", {

    expect_type(vartrack_prob_prev(p_v1 = 0.1, n = 30, omega = 0.9, precision = 0.1, c_ratio = 1.5, sampling_freq = "xsect"), "double")
})

