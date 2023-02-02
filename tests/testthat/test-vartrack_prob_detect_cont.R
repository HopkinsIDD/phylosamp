test_that("input arguments are valid", {

    expect_error(vartrack_prob_detect_cont(n = 0, t = 4, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = "invalid", t = 4, p0_v1 = 1/10000,
        r_v1 = 0.1, c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = "invalid", p0_v1 = 1/10000,
        r_v1 = 0.1, c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 0, r_v1 = 0.1,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 1.2, r_v1 = 0.1,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = TRUE, r_v1 = 0.1,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 1/1000, r_v1 = 0,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 1/1000, r_v1 = FALSE,
        c_ratio = 1.5))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 1/1000, r_v1 = 0.1,
        c_ratio = -2))

    expect_error(vartrack_prob_detect_cont(n = 30, t = 4, p0_v1 = 1/1000, r_v1 = 0.1,
        c_ratio = "invalid"))
})

test_that("return object is valid double", {

    expect_type(vartrack_prob_detect_cont(n = 40, t = 4, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = 1.5), "double")

})


test_that("manuscript results remain valid", {

    expect_gt(vartrack_prob_detect_cont(n = 158, t = 30, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = 1), 0.95)

    expect_gt(vartrack_prob_detect_cont(n = 21, t = 47, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = (0.8 * 0.975)/(0.6 * 0.95)), 0.95)
})
