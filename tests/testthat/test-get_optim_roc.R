test_that("get_optim_roc is deprecated", {
    expect_snapshot({
        x <- as.data.frame(matrix(c(1, 0, 0, 2, 0.75, 0.25, 3, 1, 1), ncol = 3, byrow = TRUE,
            dimnames = list(NULL, c("cutoff", "sensitivity", "specificity")), ))

        expect_equal(get_optim_roc(x)$cutoff, 2)
    })
})
