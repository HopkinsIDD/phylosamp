# truediscoveryrate is deprecated

    Code
      expect_equal(truediscoveryrate(eta = 1, chi = 1, rho = 1, M = 10, R = NULL,
        assumption = "stsl"), 1)
    Warning <lifecycle_warning_deprecated>
      `truediscoveryrate()` was deprecated in phylosamp 1.0.0.
      i Please use `translink_tdr()` instead.
      The `eta` argument of `truediscoveryrate()` is deprecated as of phylosamp 1.0.0.
      i Please use the `sensitivity` argument of `translink_tdr()` instead.
      The `chi` argument of `truediscoveryrate()` is deprecated as of phylosamp 1.0.0.
      i Please use the `specificity` argument of `translink_tdr()` instead.
    Message <simpleMessage>
      Calculating true discovery rate assuming single-transmission and single-linkage
    Warning <lifecycle_warning_deprecated>
      `prob_trans_stsl()` was deprecated in phylosamp 1.0.0.
      i Please use `translink_prob_transmit_stsl()` instead.
      The `eta` argument of `prob_trans_stsl()` is deprecated as of phylosamp 1.0.0.
      i Please use the `sensitivity` argument of `translink_prob_transmit_stsl()` instead.
      The `chi` argument of `prob_trans_stsl()` is deprecated as of phylosamp 1.0.0.
      i Please use the `specificity` argument of `translink_prob_transmit_stsl()` instead.

