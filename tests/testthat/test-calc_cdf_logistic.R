
test_that("input arguments are valid", {

    expect_error(
        calc_cdf_logistic(
            t="invalid",
            p0=0.1,
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0=1.1,
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0="invalid",
            r=0.1,
            c_ratio=1)
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0=0.1,
            r="invalid",
            c_ratio=1)
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0=0.1,
            r=0,
            c_ratio=1)
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio="invalid")
        )

    expect_error(
        calc_cdf_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio=0)
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_cdf_logistic(
            t=5,
            p0=0.1,
            r=0.1,
            c_ratio=1),
        "double"
        )
})

test_that("cdf increases monotonically", {

    expect_gt(
        calc_cdf_logistic(t=5, p0=0.1, r=0.1, c_ratio=1),
        calc_cdf_logistic(t=3, p0=0.1, r=0.1, c_ratio=1)
        )

    expect_lt(
        calc_cdf_logistic(t=5, p0=0.1, r=0.1, c_ratio=1.5),
        calc_cdf_logistic(t=7, p0=0.1, r=0.1, c_ratio=1.5)
        )

})

test_that("manuscript values remain valid", {

    expect_equal(
        round(
            calc_cdf_logistic(t=14, p0=1/10000, r=0.1, c_ratio=1)-
            calc_cdf_logistic(t=0, p0=1/10000, r=0.1, c_ratio=1),
            8
            ),
        0.00305473
        )

})
