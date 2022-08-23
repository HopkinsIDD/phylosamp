
test_that("input arguments are valid", {

    expect_error(
        calc_freq_logistic(
            t="invalid",
            p0=0.1,
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0=1.1,
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0="invalid",
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0=0.1,
            r="invalid",
            c_ratio=1)
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0=0.1,
            r=0,
            c_ratio=1)
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio="invalid")
        )

    expect_error(
        calc_freq_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio=0)
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_freq_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio=1),
        "double"
        )
})

test_that("variant prevalence increases when growth rate is positive", {

    expect_gt(
        calc_freq_logistic(t=5, p0=1/10000, r=0.3, c_ratio=1),
        1/10000
        )
})

test_that("variant prevalence decreases when growth rate is negative", {

    expect_lt(
        calc_freq_logistic(t=5, p0=1/10000, r=-0.3, c_ratio=1),
        1/10000
        )
})

test_that("observed variant prevalence is higher when c_ratio is above 1", {

    expect_gt(
        calc_freq_logistic(t=4, p0=1/10000, r=0.2, c_ratio=1.2),
        calc_freq_logistic(t=4, p0=1/10000, r=0.2, c_ratio=1)
        )
})

test_that("observed variant prevalence is lower when c_ratio is below 1", {

    expect_lt(
        calc_freq_logistic(t=10, p0=1/1000, r=0.11, c_ratio=.9),
        calc_freq_logistic(t=10, p0=1/1000, r=0.11, c_ratio=1)
        )
})

test_that("manuscript values remain valid", {

    expect_equal(
        round(calc_freq_logistic(t=14, p0=1/10000, r=0.1, c_ratio=1), 4),
        0.0004
        )
})

