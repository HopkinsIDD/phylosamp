test_that("translink_prob_transmit input arguments are valid", {

    expect_error(translink_prob_transmit(sensitivity = 0.9, specificity = 0.9, rho = 0.4, M = 100, R = 1, assumption = "fake"), 
        "assumption argument")
    
})

test_that("translink_prob_transmit return object is valid double", {

    expect_type(translink_prob_transmit(sensitivity = 0.9, specificity = 0.9, rho = 0.4, M = 100, R = 1, assumption = "stsl"),
        "double")
    expect_type(translink_prob_transmit(sensitivity = 0.9, specificity = 0.9, rho = 0.4, M = 100, R = 1, assumption = "mtsl"),
        "double")
    expect_type(translink_prob_transmit(sensitivity = 0.9, specificity = 0.9, rho = 0.4, M = 100, R = 1, assumption = "mtml"),
        "double")
 
})

