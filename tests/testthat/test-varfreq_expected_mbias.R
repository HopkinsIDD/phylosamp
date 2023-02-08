
test_that("varfreq_expected_mbias input arguments are valid", {

    expect_error(varfreq_expected_mbias(p_v1 = 0, c_ratio = 1))

    expect_error(varfreq_expected_mbias(p_v1 = TRUE, c_ratio = 1))

    expect_error(varfreq_expected_mbias(p_v1 = 0.1, c_ratio = -0.1))

    expect_error(varfreq_expected_mbias(p_v1 = 0.1, c_ratio = "invalid"))
})

test_that("varfreq_expected_mbias return object is valid double", {

    expect_type(varfreq_expected_mbias(p_v1 = 0.2, c_ratio = 1.5), "double")

})

test_that("varfreq_expected_mbias there is no bias if c_ratio is 1", {

    expect_equal(varfreq_expected_mbias(p_v1 = 0.44, c_ratio = 1), 1)
})
