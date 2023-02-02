
test_that("input arguments are valid", {

    expect_error(
        calc_observed_freq(
            p_v1=0,
            c_ratio=1)
        )

    expect_error(
        calc_observed_freq(
            p_v1=1.1,
            c_ratio=1)
        )

    expect_error(
        calc_observed_freq(
            p_v1="invalid",
            c_ratio=1)
        )

    expect_error(
        calc_observed_freq(
            p_v1=0.1,
            c_ratio=-2
            )
        )

    expect_error(
        calc_observed_freq(
            p_v1=0.1,
            c_ratio=FALSE
            )
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_observed_freq(
            p_v1=0.2,
            c_ratio=1.5),
        "double"
        )

})

test_that("observed variant prevalence is equal to actual variant prevalence if c_ratio is 1", {

    expect_equal(
        calc_observed_freq(p_v1=0.8, c_ratio=1),
        0.8
        )
})

test_that("observed_variant prevalence is greater than actual variant prevalence if c_ratio is above 1", {

    expect_gt(
        calc_observed_freq(p_v1=0.4, c_ratio=2),
        0.4
        )

})

test_that("observed variant prevalence is less than actual variant prevalence if c_ratio is below 1", {

    expect_lt(
        calc_observed_freq(p_v1=0.75, c_ratio=0.3),
        0.75
        )

})

test_that("manuscript results remain valid", {

    expect_equal(
        round(calc_observed_freq(p_v1=0.02, c_ratio=(0.975*0.8)/(0.6*0.95)), 3),
        0.027
        )
})

