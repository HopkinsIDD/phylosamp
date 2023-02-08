test_that("vartrack_samplesize_prev input arguments are valid", {

    expect_error(vartrack_samplesize_prev(p_v1 = 0.1, prob = 0.9, precision = 0.1, omega = 0.9, c_ratio = 1.5, sampling_freq = "fake"),
        "sampling frequency argument")
    expect_error(vartrack_samplesize_prev(p_v1 = 0.2, c_ratio = 1, sampling_freq = "xsect"),
        "missing")
    expect_error(vartrack_samplesize_prev(p_v1 = 0.1, omega = 0.9, c_ratio = 1.5, sampling_freq = "cont"),
        "not yet implemented")
})

test_that("vartrack_samplesize_prev return object is valid double", {

    expect_type(vartrack_samplesize_prev(p_v1 = 0.1, prob = 0.9, omega = 0.9, precision = 0.1, c_ratio = 1.5, sampling_freq = "xsect"), "double")
})

