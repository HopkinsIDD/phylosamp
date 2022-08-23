
test_that("input arguments are valid", {

    expect_error(
        calc_samplesize_prev(
            p_v1=0,
            prob=0.4,
            precision=0.05,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=1.1,
            prob=0.9,
            precision=0.05,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1="invalid",
            prob=0.7,
            precision=0.05,
            c_ratio=1)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=-0.2,
            precision=0.05,
            c_ratio=1.3)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=1,
            precision=0.05,
            c_ratio=1.5)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob="invalid",
            precision=0.05,
            c_ratio=.9)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=0.95,
            precision=0,
            c_ratio=1.3)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=0.95,
            precision=1.2,
            c_ratio=1.5)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=0.95,
            precision="invalid",
            c_ratio=.9)
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.1,
            prob=0.3,
            precision=0.05,
            c_ratio=0
            )
        )

    expect_error(
        calc_samplesize_prev(
            p_v1=0.1,
            prob=0.2,
            precision=0.05,
            c_ratio="invalid"
            )
        )
})

test_that("return object is valid double", {

    expect_type(
        calc_samplesize_prev(
            p_v1=0.2,
            prob=0.9,
            precision=0.02,
            c_ratio=1.5),
        "double"
        )
})

test_that("manuscript results remain valid", {

    expect_equal(
        ceiling(
            calc_samplesize_prev(
                p_v1=0.1,
                prob=0.95,
                precision=0.25,
                c_ratio=1
                )
            ),
        554
        )

    expect_equal(
        ceiling(
            calc_samplesize_prev(
                p_v1=0.1,
                prob=0.95,
                precision=0.25,
                c_ratio=1/0.84
                )
            ),
        464
        )
})
