
test_that("input arguments are valid", {

    expect_error(varfreq_expected_mbias(p_v1 = 0, c_ratio = 1))

    expect_error(varfreq_expected_mbias(p_v1 = TRUE, c_ratio = 1))

    expect_error(varfreq_expected_mbias(p_v1 = 0.1, c_ratio = -0.1))

    expect_error(varfreq_expected_mbias(p_v1 = 0.1, c_ratio = "invalid"))
})

test_that("return object is valid double", {

    expect_type(varfreq_expected_mbias(p_v1 = 0.2, c_ratio = 1.5), "double")

})

test_that("there is no bias if c_ratio is 1", {

    expect_equal(varfreq_expected_mbias(p_v1 = 0.44, c_ratio = 1), 1)
})
