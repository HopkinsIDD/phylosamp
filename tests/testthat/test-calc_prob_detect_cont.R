test_that("input arguments are valid", {

    expect_error(
        calc_prob_detect_cont(
            n=0,
            t=4,
            p0=1/10000,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n="invalid",
            t=4,
            p0=1/10000,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t="invalid",
            p0=1/10000,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=0,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=1.2,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=TRUE,
            r=0.1,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=1/1000,
            r=0,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=1/1000,
            r=FALSE,
            c_ratio=1.5)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=1/1000,
            r=0.1,
            c_ratio=-2)
        )

    expect_error(
        calc_prob_detect_cont(
            n=30,
            t=4,
            p0=1/1000,
            r=0.1,
            c_ratio="invalid")
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_prob_detect_cont(
            n=40,
            t=4,
            p0=1/10000,
            r=0.1,
            c_ratio=1.5),
        "double"
        )

})


test_that("manuscript results remain valid", {

    expect_gt(
        calc_prob_detect_cont(
            n=158,
            t=14,
            p0=1/10000,
            r=0.1,
            c_ratio=1
            ),
        0.95
        )

    expect_gt(
        calc_prob_detect_cont(
            n=21,
            t=47,
            p0=1/10000,
            r=0.1,
            c_ratio=(0.8*0.975)/(0.6*0.95)
            ),
        0.95
        )
})