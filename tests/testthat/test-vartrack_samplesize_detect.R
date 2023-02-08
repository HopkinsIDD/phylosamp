test_that("vartrack_samplesize_detect input arguments are valid", {

    expect_error(vartrack_samplesize_detect(prob = 0.2, p_v1 = 0.1, omega = 0.9, c_ratio = 1.5, sampling_freq = "fake"),
        "sampling frequency argument")
    expect_error(vartrack_samplesize_detect(prob = 0.3, omega = 0.9, c_ratio = 1, sampling_freq = "xsect"),
        "the xsect method")
    expect_error(vartrack_samplesize_detect(prob = 0.9, p_v1 = 0.1, omega = 0.9, c_ratio = 1.5, sampling_freq = "cont"),
        "the cont method")
})

test_that("vartrack_samplesize_detect return object is valid double", {

    expect_type(vartrack_samplesize_detect(p_v1 = 0.1, prob = 0.9, omega = 0.9, c_ratio = 1.5, sampling_freq = "xsect"), "double")
    expect_type(vartrack_samplesize_detect(prob = 0.9, t = 2, omega = 0.8, p0_v1 = 0.02, r_v1 = 0.002, c_ratio = 1, sampling_freq = "cont"), "double")
    expect_type(vartrack_samplesize_detect(prob = 0.9, p_v1 = 0.022, omega = 0.8, p0_v1 = 0.02, r_v1 = 0.002, c_ratio = 1, sampling_freq = "cont"), "double")
})

