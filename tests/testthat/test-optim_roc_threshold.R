
test_that("point nearest (0,1) corner found", {

    x <- as.data.frame(matrix(c(1, 0, 0, 2, 0.75, 0.25, 3, 1, 1), ncol = 3, byrow = TRUE,
        dimnames = list(NULL, c("cutoff", "sensitivity", "specificity")), ))

    expect_equal(optim_roc_threshold(x)$cutoff, 2)
})
