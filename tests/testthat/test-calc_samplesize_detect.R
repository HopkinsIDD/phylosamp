
test_that("input arguments are valid", {

    expect_error(
        calc_samplesize_detect(
            p_v1=0,
            prob=0.4,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=1.1,
            prob=0.9,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1="invalid",
            prob=0.7,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=0.2,
            prob=-0.2,
            c_ratio=1.3)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=0.2,
            prob=1,
            c_ratio=1.5)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=0.2,
            prob="invalid",
            c_ratio=.9)
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=0.1,
            prob=0.3,
            c_ratio=0
            )
        )

    expect_error(
        calc_samplesize_detect(
            p_v1=0.1,
            prob=0.2,
            c_ratio="invalid"
            )
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_samplesize_detect(
            p_v1=0.2,
            prob=0.9,
            c_ratio=1.5),
        "double"
        )

})

test_that("manuscript results remain valid", {

    expect_equal(
        ceiling(
            calc_samplesize_detect(p_v1=0.02, prob=0.95, c_ratio=1)),
        149
        )

    expect_equal(
        ceiling(
            calc_samplesize_detect(p_v1=0.02, prob=0.95, c_ratio=(.8*.975)/(.6*.95))),
        110
        )
})

