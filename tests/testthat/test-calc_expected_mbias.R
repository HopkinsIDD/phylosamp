
test_that("input arguments are valid", {

    expect_error(
        calc_expected_mbias(
            p_v1=0,
            c_ratio=1)
        )

    expect_error(
        calc_expected_mbias(
            p_v1="invalid",
            c_ratio=1)
        )

    expect_error(
        calc_expected_mbias(
            p_v1=0.1,
            c_ratio=0
            )
        )

    expect_error(
        calc_expected_mbias(
            p_v1=0.1,
            c_ratio="invalid"
            )
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_expected_mbias(
            p_v1=0.2,
            c_ratio=1.5),
        "double"
        )

})

test_that("actual variant prevalence is equal to observed variant prevalence if c_ratio is 1", {

    expect_equal(
        calc_expected_mbias(
            p_v1=0.44,
            c_ratio=1
            ),
        1
        )
})